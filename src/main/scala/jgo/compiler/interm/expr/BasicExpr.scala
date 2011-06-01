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
private class BasicExpr(evalCode: => CodeBuilder, val typeOf: Type) extends Expr {
  def eval = evalCode
}

private object BasicExpr {
  def apply(eval: => CodeBuilder, t: Type) = new BasicExpr(eval, t)
  def unapply(e: Expr): Option[(CodeBuilder, Type)] = e match {
    case se: BasicExpr => Some(se.eval, se.typeOf)
    case _ => None
  }
}
