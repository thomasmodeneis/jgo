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

private class EvalExpr(evalCode: => CodeBuilder, val typeOf: Type) extends BasicExpr with UnderlyingFromEvalExpr {
  def eval = evalCode
}
private class UnderlyingExpr(evalUnderCode: => CodeBuilder, val typeOf: Type) extends BasicExpr with EvalFromUnderlyingExpr {
  def evalUnder = evalUnderCode
}

private object EvalExpr {
  def apply(eval: => CodeBuilder, t: Type) = new EvalExpr(eval, t)
}
private object UnderlyingExpr {
  def apply(eval: => CodeBuilder, t: Type) = new UnderlyingExpr(eval, t)
}
