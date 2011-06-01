package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

trait ConstTypeCheckOverrides extends TypeChecks {
  override def boolExpr(e: Expr, desc: String) (implicit pos: Pos) = e match {
    case BoolConst(b) => new BoolValueExpr(PushBool(b))
    case _ => super.boolExpr(e, desc)
  }
  
  protected override def same[T <: Type](e1: Expr, e2: Expr)
                                        (f: (Expr, String) => M[(Expr, T)])
                                        (implicit pos: Pos) =
    (e1, e2) match {
      case (e, u: UntypedConst) =>
        val eRightM = u.withType(e.typeOf) match {
          case Some(eRight) => Result(eRight)
          case None => Problem(
            "right operand %s (untyped) is not compatible with left operand's type %s", u, e.typeOf)
        }
        for (((eLeft, t), eRight) <- (f(e, "left operand"), eRightM))
        yield (eLeft, eRight, t)
      
      case (u: UntypedConst, e) =>
        val eLeftM = u.withType(e.typeOf) match {
          case Some(eRight) => Result(eRight)
          case None => Problem(
            "left operand %s (untyped) is not compatible with right operand's type %s", u, e.typeOf)
        }
        for ((eLeft, (eRight, t)) <- (eLeftM, f(e, "right operand")))
        yield (eLeft, eRight, t)
      
      case _ => super.same(e1, e2)(f)
    }
}
