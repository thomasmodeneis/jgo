package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

private trait ConstTypeCheckOverrides extends TypeChecks {
  abstract override def boolExpr(e: Expr, desc: String) (implicit pos: Pos) = e match {
    case BoolConst(b) => new BoolValueExpr(PushBool(b))
    case _ => super.boolExpr(e, desc)
  }
}
