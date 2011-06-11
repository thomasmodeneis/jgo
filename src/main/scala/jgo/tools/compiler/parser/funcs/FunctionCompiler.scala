package jgo.tools.compiler
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
  
  val target = new Function(funcName, sig.typeOf)
  
  def hasNamedResults = sig.hasNamedResults
  
  for (param <- sig.namedParams) 
    growable.put(param.name, param)
  for (result <- sig.namedResults)
    growable.put(result.name, result)
  
  lazy val compile: Err[FunctionInterm] = {
    val codeBuilderErr = parse.block(inAtBrace) match {
      case Success(codeBuilderErr, _) => codeBuilderErr
      case NoSuccess(msg, in) => problem("syntax error in function %s: %s", funcName, msg)(in.pos)
    }
    for (codeBuilder <- codeBuilderErr) yield
      new FunctionInterm(target, sig, codeBuilder.result)
  }
}
