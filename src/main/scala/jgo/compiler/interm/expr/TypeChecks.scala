package jgo.compiler
package interm
package expr

import message._
import message.Messaged._

import types._
import instr._
import instr.TypeConversions._
import codeseq._

private trait TypeChecks {
  protected def boolExpr(e: Expr, desc: String) (implicit pos: Pos): M[BoolExpr] = e match {
    case b: BoolExpr => Result(b)
    case HasType(BoolType) => Problem("%s has type %s; boolean newtypes not yet supported", desc, e.t)
    case _ => Problem("%s has type %s; boolean type required", desc, e.t)
  }
  protected def sameBoolExpr(e1: Expr, e2: Expr, d1: String, d2: String) (implicit pos: Pos): M[(BoolExpr, BoolExpr)] = for {
    (b1, b2) <- together(boolExpr(e1, d1), boolExpr(e2, d2))
    result <- if (e1.t == e2.t) Result(b1, b2)
              else Problem("left and right operands have differing types %s and %s", e1.t, e2.t)
  } yield result
  
  
  protected def addable(e: Expr, desc: String) (implicit pos: Pos): M[AddableType] = e match {
    case HasType(ut: AddableType) => Result(ut)
    case _ => Problem("%s has type %s; numeric or string type required", desc, e.t)
  }
  protected def numeric(e: Expr, desc: String) (implicit pos: Pos): M[NumericType] = e match {
    case HasType(ut: NumericType) => Result(ut)
    case _ => Problem("%s has type %s; numeric type required", desc, e.t)
  }
  protected def integral(e: Expr, desc: String) (implicit pos: Pos): M[IntegralType] = e match {
    case HasType(ut: IntegralType) => Result(ut)
    case _ => Problem("%s has type %s; integral type required", desc, e.t)
  }
  protected def unsigned(e: Expr, desc: String) (implicit pos: Pos): M[UnsignedType] = e match {
    case HasType(ut: UnsignedType) => Result(ut)
    case _ => Problem("%s has type %s; unsigned integral type required", desc, e.t)
  }
  protected def string(e: Expr, desc: String) (implicit pos: Pos): M[Unit] = e match {
    case HasType(StringType) => Result(())
    case _ => Problem("%s has type %s; string type required", desc, e.t)
  }
  
  
  protected def sameType(e1: Expr, e2: Expr, d1: String, d2: String) (implicit pos: Pos): M[Type] =
    if (e1.t == e2.t)
      Result(e1.t)
    else
      Problem("left and right operands have differing types %s and %s", e1.t, e2.t)
  
  
  protected def sameAddable(e1: Expr, e2: Expr, d1: String, d2: String) (implicit pos: Pos): M[AddableType] = for {
    (ut1, ut2) <- together(addable(e1, d1), addable(e2, d2))
    result <- if (e1.t == e2.t) Result(ut1)
              else Problem("left and right operands have differing types %s and %s", e1.t, e2.t)
  } yield result
  
  protected def sameNumeric(e1: Expr, e2: Expr, d1: String, d2: String) (implicit pos: Pos): M[NumericType] = for {
    (ut1, ut2) <- together(numeric(e1, d1), numeric(e2, d2))
    result <- if (e1.t == e2.t) Result(ut1)
              else Problem("left and right operands have differing types %s and %s", e1.t, e2.t)
  } yield result
  
  protected def sameIntegral(e1: Expr, e2: Expr, d1: String, d2: String) (implicit pos: Pos): M[IntegralType] = for {
    (ut1, ut2) <- together(integral(e1, d1), integral(e2, d2))
    result <- if (e1.t == e2.t) Result(ut1)
              else Problem("left and right operands have differing types %s and %s", e1.t, e2.t)
  } yield result
  
  protected def sameUnsigned(e1: Expr, e2: Expr, d1: String, d2: String) (implicit pos: Pos): M[UnsignedType] = for {
    (ut1, ut2) <- together(unsigned(e1, d1), unsigned(e2, d2))
    result <- if (e1.t == e2.t) Result(ut1)
              else Problem("left and right operands have differing types %s and %s", e1.t, e2.t)
  } yield result
  
  protected def sameString(e1: Expr, e2: Expr, d1: String, d2: String) (implicit pos: Pos): M[Unit] = for {
    (_, _) <- together(string(e1, d1), string(e2, d2))
    result <- if (e1.t == e2.t) Result(())
              else Problem("left and right operands have differing types %s and %s", e1.t, e2.t)
  } yield result
  
  
  protected def recvChanT(e: Expr) (implicit pos: Pos): M[Type] = e match {
    case HasType(RecvChanType(t)) => Result(t)
    case _ => Problem("operand of channel receive has type %s; receiving chan type required", e.t)
  }
  protected def sendChanT(e: Expr) (implicit pos: Pos): M[Type] = e match {
    case HasType(SendChanType(t)) => Result(t)
    case _ => Problem("left operand of channel send has type %s; sending chan type required", e.t)
  }
}
