package jgo.compiler
package interm
package expr

import message._
import message.Messaged._

import types._
import instr._
import instr.TypeConversions._
import codeseq._

private trait ArithmeticTypeChecks {
  private[expr] def addable(e: Expr, desc: String) (implicit pos: Pos): M[AddableType] = e match {
    case HasType(ut: AddableType) => Result(ut)
    case _ => Problem("%s has type %s; numeric or string type required", desc, e.t)
  }
  private[expr] def numeric(e: Expr, desc: String) (implicit pos: Pos): M[NumericType] = e match {
    case HasType(ut: NumericType) => Result(ut)
    case _ => Problem("%s has type %s; numeric type required", desc, e.t)
  }
  private[expr] def integral(e: Expr, desc: String) (implicit pos: Pos): M[IntegralType] = e match {
    case HasType(ut: IntegralType) => Result(ut)
    case _ => Problem("%s has type %s; integral type required", desc, e.t)
  }
  private[expr] def unsigned(e: Expr, desc: String) (implicit pos: Pos): M[UnsignedType] = e match {
    case HasType(ut: UnsignedType) => Result(ut)
    case _ => Problem("%s has type %s; unsigned integral type required", desc, e.t)
  }
  
  private[expr] def sameAddable(e1: Expr, e2: Expr, d1: String, d2: String) (implicit pos: Pos): M[AddableType] = for {
    (ut1, ut2) <- together(addable(e1, d1), addable(e2, d2))
    _ <- if (e1.t == e2.t) Result(())
         else Problem("left and right operands have differing types %s and %s", e1.t, e2.t)
  } yield ut1
  
  private[expr] def sameNumeric(e1: Expr, e2: Expr, d1: String, d2: String) (implicit pos: Pos): M[NumericType] = for {
    (ut1, ut2) <- together(numeric(e1, d1), numeric(e2, d2))
    _ <- if (e1.t == e2.t) Result(())
         else Problem("left and right operands have differing types %s and %s", e1.t, e2.t)
  } yield ut1
  
  private[expr] def sameIntegral(e1: Expr, e2: Expr, d1: String, d2: String) (implicit pos: Pos): M[IntegralType] = for {
    (ut1, ut2) <- together(integral(e1, d1), integral(e2, d2))
    _ <- if (e1.t == e2.t) Result(())
         else Problem("left and right operands have differing types %s and %s", e1.t, e2.t)
  } yield ut1
  
  private[expr] def sameUnsigned(e1: Expr, e2: Expr, d1: String, d2: String) (implicit pos: Pos): M[UnsignedType] = for {
    (ut1, ut2) <- together(unsigned(e1, d1), unsigned(e2, d2))
    _ <- if (e1.t == e2.t) Result(())
         else Problem("left and right operands have differing types %s and %s", e1.t, e2.t)
  } yield ut1
}
