package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

private class SimpleExpr(evalCode: => CodeBuilder, val typeOf: Type) extends Expr {
  def eval = evalCode
}

private object SimpleExpr {
  def apply(eval: => CodeBuilder, t: Type) = new SimpleExpr(eval, t)
  def unapply(e: Expr): Option[(CodeBuilder, Type)] = e match {
    case se: SimpleExpr => Some(se.eval, se.t)
    case _ => None
  }
}
