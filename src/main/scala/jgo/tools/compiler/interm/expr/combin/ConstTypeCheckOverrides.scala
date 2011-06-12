package jgo.tools.compiler
package interm
package expr
package combin

import types._
import instr._
import instr.TypeConversions._
import codeseq._

trait ConstTypeCheckOverrides extends TypeChecks {
  protected override def condExpr(e: Expr, desc: String)(implicit pos: Pos) = e match {
    case TypedBoolConst(b, t) => result(new CondValueExpr(PushBool(b), t))
    case UntypedBoolConst(b)  => result(new CondValueExpr(PushBool(b), scope.UniverseScope.bool))
    case _ => super.condExpr(e, desc)
  }
  
  protected override def same[T <: Type](f: (Expr, String) => Err[T])
                                             (e1: Expr, e2: Expr)
                                             (implicit pos: Pos) =
    (e1, e2) match {
      case (e, u: UntypedConst) =>
        val eRightErr = u.withType(e.typeOf) match {
          case Some(eRight) => result(eRight)
          case None => problem(
            "right operand %s (untyped) is not compatible with left operand's type %s", u, e.typeOf)
        }
        for ((t, eRight) <- (f(e, "left operand"), eRightErr))
        yield (e, eRight, t)
      
      case (u: UntypedConst, e) =>
        val eLeftErr = u.withType(e.typeOf) match {
          case Some(eRight) => result(eRight)
          case None => problem(
            "left operand %s (untyped) is not compatible with right operand's type %s", u, e.typeOf)
        }
        for ((eLeft, t) <- (eLeftErr, f(e, "right operand")))
        yield (eLeft, e, t)
      
      case _ => super.same(f)(e1, e2)
    }
}
