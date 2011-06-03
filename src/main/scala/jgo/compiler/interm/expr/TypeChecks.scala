package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

/**
 * Provides type checking utilities common to the several
 * Combinators implementations.
 */
trait TypeChecks {
  protected def condExpr(e: Expr, desc: String) (implicit pos: Pos): M[ConditionalExpr] = e match {
    case b: ConditionalExpr => Result(b)
    case HasType(BoolType)  => Result(new CondValueExpr(e.evalUnder, e.typeOf))
    case _ => Problem("%s has type %s; boolean type required", desc, e.typeOf)
  }
  
  protected def sameCondExpr(e1: Expr, e2: Expr) (implicit pos: Pos): M[(ConditionalExpr, ConditionalExpr)] =
    for {
      (b1, b2) <- (condExpr(e1, "left operand"), condExpr(e2, "right operand"))
      result <- if (e1.typeOf == e2.typeOf) Result(b1, b2)
                else Problem("left and right operands have differing types %s and %s", e1.typeOf, e2.typeOf)
    } yield result
  
  
  protected def addable(e: Expr, desc: String) (implicit pos: Pos): M[AddableType] = e match {
    case HasType(ut: AddableType) => Result(ut)
    case _ => Problem("%s has type %s; numeric or string type required", desc, e.typeOf)
  }
  protected def numeric(e: Expr, desc: String) (implicit pos: Pos): M[NumericType] = e match {
    case HasType(ut: NumericType) => Result(ut)
    case _ => Problem("%s has type %s; numeric type required", desc, e.typeOf)
  }
  protected def integral(e: Expr, desc: String) (implicit pos: Pos): M[IntegralType] = e match {
    case HasType(ut: IntegralType) => Result(ut)
    case _ => Problem("%s has type %s; integral type required", desc, e.typeOf)
  }
  protected def unsigned(e: Expr, desc: String) (implicit pos: Pos): M[UnsignedType] = e match {
    case HasType(ut: UnsignedType) => Result(ut)
    case _ => Problem("%s has type %s; unsigned integral type required", desc, e.typeOf)
  }
  protected def string(e: Expr, desc: String) (implicit pos: Pos): M[StringType.type] = e match {
    case HasType(StringType) => Result(StringType)
    case _ => Problem("%s has type %s; string type required", desc, e.typeOf)
  }
  
  
  protected def sameType(e1: Expr, e2: Expr) (implicit pos: Pos): M[Type] =
    if (e1.typeOf == e2.typeOf)
      Result(e1.typeOf)
    else
      Problem("left and right operands have differing types %s and %s", e1.typeOf, e2.typeOf)
  
  
  protected def same[T <: UnderType](e1: Expr, e2: Expr)(f: (Expr, String) => M[T])(implicit pos: Pos) =
    for {
      (t1, t2) <- (f(e1, "left operand"), f(e2, "right operand"))
      result <- if (e1.typeOf == e2.typeOf) Result((e1, e2, t1))
                else Problem("left and right operands have differing types %s and %s", e1.typeOf, e2.typeOf)
    } yield result
  
  protected def sameAddable(e1: Expr, e2: Expr) (implicit pos: Pos): M[(Expr, Expr, AddableType)] =
    same(e1, e2)(addable)
  
  protected def sameNumeric(e1: Expr, e2: Expr) (implicit pos: Pos): M[(Expr, Expr, NumericType)] =
    same(e1, e2)(numeric)
  
  protected def sameIntegral(e1: Expr, e2: Expr) (implicit pos: Pos): M[(Expr, Expr, IntegralType)] =
    same(e1, e2)(integral)
  
  protected def sameUnsigned(e1: Expr, e2: Expr) (implicit pos: Pos): M[(Expr, Expr, UnsignedType)] =
    same(e1, e2)(unsigned)
  
  protected def sameString(e1: Expr, e2: Expr) (implicit pos: Pos): M[(Expr, Expr)] =
    for ((s1, s2, _) <- same(e1, e2)(string))
    yield (s1, s2)
  
  
  protected def recvChanT(e: Expr, desc: String) (implicit pos: Pos): M[Type] = e match {
    case HasType(RecvChanType(t)) => Result(t)
    case _ => Problem("%s has type %s; receiving chan type required", desc, e.typeOf)
  }
  protected def sendChanT(e: Expr, desc: String) (implicit pos: Pos): M[Type] = e match {
    case HasType(SendChanType(t)) => Result(t)
    case _ => Problem("%s has type %s; sending chan type required", desc, e.typeOf)
  }
}
