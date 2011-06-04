package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

/**
 * An "ordinary" expression which requires no special processing.
 */
private sealed abstract class BasicExpr extends Expr

/**
 * A basic expression whose evaluation code and type are as specified.
 * 
 * We receive the evaluation code by name to ensure that `eval` returns
 * a new `CodeBuilder` instance on each invocation (provided, of course,
 * that the passed argument results in a new instance on each evaluation).
 *
 * @param evalCode the evaluation code for this expression
 * @param typeOf   the type of this expression
 */
private class EvalExpr(evalCode: => CodeBuilder, val typeOf: Type) extends BasicExpr with UnderlyingFromEvalExpr {
  def eval = evalCode
}

/**
 * A basic expression whose eval-underlying code and type are as specified.
 * 
 * We receive the eval-underlying code by name to ensure that `evalUnder`
 * returns a new `CodeBuilder` instance on each invocation (provided, of
 * course, that the passed argument results in a new instance on each
 * evaluation).
 * 
 * @param evalUnderCode the eval-underlying code for this expression
 * @param typeOf        the type of this expression
 */
private class UnderlyingExpr(evalUnderCode: => CodeBuilder, val typeOf: Type) extends BasicExpr with EvalFromUnderlyingExpr {
  def evalUnder = evalUnderCode
}

private object EvalExpr {
  def apply(eval: => CodeBuilder, t: Type) = new EvalExpr(eval, t)
}
private object UnderlyingExpr {
  def apply(eval: => CodeBuilder, t: Type) = new UnderlyingExpr(eval, t)
}
