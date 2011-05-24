package jgo.compiler
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

class CompilationUnitCompiler(in: Input) extends Declarations with GrowablyScoped {
  def growable = SequentialScope.base(UniverseScope)
  def scope = growable
  
  private val functionCompilers: mut.Map[String, FunctionCompiler] = mut.Map.empty
  
  protected def mkVariable(name: String, typeOf: Type) =
    (new GlobalVar(name, typeOf), CodeBuilder.empty)
  
  
  private lazy val file =
    repWithSemi(topLevelDecl)
  
  private lazy val topLevelDecl =
    declaration | function
  
  private lazy val function =
    "func" ~>! ident ~ signature ~ inputAt("{") <~ skipBlock  ^^ { case name ~ sigM ~ in =>
      for (sig <- sigM) {
        val v = new FunctionCompiler(name, sig, scope, in)
        growable.put(name, v.target)
        functionCompilers.put(name, v)
      }
    }
  
  private val skipBlock = Parser { in =>
    var cur = in
    var level = 0
    do {
      if (cur.first == Keyword("{"))
        level += 1
      else if (cur.first == Keyword("}"))
        level -= 1
    } while (level > 0)
    Success((), cur.rest)
  }
}
