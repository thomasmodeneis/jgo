package jgo.compiler
package parser
package funcs

import scoped._
import stmts._
import exprs._
import types._

import scope._
import interm._
import interm.types._
import instr._
import codeseq._
import symbol._

//TODO:  Clean this up.
final class FunctionCompiler(funcName: String, sig: Signature, encl: Scope, inAtBrace: Input) extends FuncCompiler with Statements {
  //for clarity
  parse: Statements =>
  
  protected def initialEnclosing = encl
  
  val target = new Function(funcName, sig.typeOf)
  def hasNamedResults = sig.hasNamedResults
  
  for (param <- sig.namedParams) 
    growable.put(param.name, param)
  for (result <- sig.namedResults)
    growable.put(result.name, result)
  
  /*
  lazy val inAfter: Input = {
    import lexer._
    
    var cur = inAtBrace
    var nestingLvl = 1 //TODO:  Add logic for syntax errors in signature
    
    while (nestingLvl > 0) {
      cur = cur.rest
      if (cur.first == Keyword("{"))
        nestingLvl += 1
      if (cur.first == Keyword("}"))
        nestingLvl -= 1
    }
    
    cur.rest
  }*/
  
  lazy val compile: M[FunctionInterm] = {
    val codeBuilderM = parse.block(inAtBrace) match {
      case Success(codeBuilderM, _) => codeBuilderM
      case NoSuccess(msg, in) => Problem("syntax error in function %s: %s", funcName, msg)(in.pos)
    }
    
    for (codeBuilder <- codeBuilderM) yield
      new FunctionInterm(target, sig, codeBuilder.result)
  }
}
