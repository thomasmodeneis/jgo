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

final class FunctionCompiler(funcName: String, sig: Signature, encl: Scope, inAtBrace: Input) extends FuncCompiler with Statements {
  //for clarity
  parse: Statements =>
  
  protected def initialEnclosing = encl
  
  def target = new Function(funcName, sig.typeOf)
  def hasNamedResults = sig.hasNamedResults
  
  for (param <- sig.namedParams) 
    growable.put(param.name, param)
  for (result <- sig.namedResults)
    growable.put(result.name, result)
  
  lazy val compile: M[FunctionInterm] = {
    val codeBuilderM = parse.block(inAtBrace) match {
      case Success(codeBuilderM, _) => codeBuilderM
      case NoSuccess(msg, in) => Problem("syntax error in function %s: %s", funcName, msg)(in.pos)
    }
    
    for (codeBuilder <- codeBuilderM) yield
      new FunctionInterm(target, sig, codeBuilder.result)
  }
}
