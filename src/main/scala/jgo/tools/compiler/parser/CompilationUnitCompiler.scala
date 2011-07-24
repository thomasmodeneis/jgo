package jgo.tools.compiler
package parser

import funcs._
import stmts._
import scoped._

import lexer._
import scope._
import interm._
import interm.types._
import symbol._
import codeseq._

import scala.collection.{mutable => mut}

class CompilationUnitCompiler(target: Package, in: Input) extends TopLevel with GrowablyScoped {
  val growable = StackScope.base(UniverseScope)
  def scope = growable
  
  private[this] val functionCompilers: mut.Map[Function, FunctionCompiler] = mut.Map.empty
  private[this] val definedTypes: mut.Map[String, WrappedType] = mut.Map.empty
  private[this] var globalVars: List[GlobalVar] = Nil
  private[this] var initCode = CodeBuilder.empty
  
  protected override def registerTypeDecl(name: String, t: WrappedType) {
    definedTypes(name) = t
  }
  
  protected override def registerVarDecl(name: String, v: GlobalVar) {
    globalVars ::= v
  }
  
  protected override def addInitCode(code: CodeBuilder) {
    initCode = initCode |+| code
  }
  
  //Be sure execute this only once.
  private def parseTopLevel(): Err[Unit] = extractFromParseResult(phrase(parseFile)(in)) withResult ()
  
  lazy val compile: Err[PkgInterm] = {
    val topLevelErrs = parseTopLevel()
    
    val functionsErr = {
      val functionMap = mut.Map[Function, FunctionInterm]()
      var errors: Err[Any] = result(())
      for ((f, fCompl) <- functionCompilers) {
        val fIntermErr = fCompl.compile
        errors = errors then fIntermErr
        for (fInterm <- fIntermErr)
          functionMap.put(f, fInterm)
      }
      for (_ <- errors)
      yield functionMap.toMap //to immutable map
    }
    
    //topLevelErrs holds all of the top-level errors, if any;
    //functionsErr holds all of the function-level ones
    for ((_, functions) <- (topLevelErrs, functionsErr))
    yield PkgInterm(target, definedTypes.values.toList, functions, globalVars, initCode.result)
  }
  
  
  private lazy val parseFile: Rule[Unit] =                               "file" $
    repWithSemi(topLevelDecl)  ^^ { _ withResult () }
  
  private lazy val topLevelDecl: Rule[Unit] =           "top level declaration" $
    (declaration | function)
  
  private lazy val function: Rule[Unit] =                       "function decl" $
    "func" ~! ident ~ signature ~ inputAt(skipBlock)  ^^ { case pos ~ name ~ sigErr ~ in =>
      sigErr flatMap { sig =>
        val funcCompl = new FunctionCompiler(name, sig, scope, in)
        functionCompilers.put(funcCompl.target, funcCompl)
        //add to the scope
        bind(name, funcCompl.target)(pos) withResult ()
      }
    }
  
  //TODO:  Add EOF detection.
  private lazy val skipBlock = Parser { in =>
    var cur = in
    var level = 0
    do {
      if (cur.first == Keyword("{"))
        level += 1
      else if (cur.first == Keyword("}"))
        level -= 1
      cur = cur.rest
    } while (level > 0)
    Success(Unit, cur)
  }
}
