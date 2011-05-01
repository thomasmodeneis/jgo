package jgo.compiler
package interm
package expr

import message._
import message.Messaged._

import types._
import instr._
import instr.TypeConversions._
import codeseq._

import scala.util.parsing.input.Position

private trait BasicCombinators extends Combinators {
  private type M[+T] = Messaged[T]
  private type Pos = Position
  
  private def addable(eM: M[Expr], desc: String) (implicit pos: Pos): M[(Expr, AddableType)] = for {
    e <- eM
    y <- e match {
      case HasType(t: AddableType) => Result(e, t)
      case HasType(t)  => Problem("%s is of type %s; numeric or string type required", desc, t)
    }
  } yield y
  private def numeric(eM: M[Expr], desc: String) (implicit pos: Pos): M[(Expr, NumericType)] = for {
    e <- eM
    y <- e match {
      case HasType(t: NumericType) => Result(e, t)
      case HasType(t)  => Problem("%s is of type %s; numeric type required", desc, t)
    }
  } yield y
  private def integral(eM: M[Expr], desc: String) (implicit pos: Pos): M[(Expr, IntegralType)] = for {
    e <- eM
    y <- e match {
      case HasType(t: IntegralType) => Result(e, t)
      case HasType(t)  => Problem("%s is of type %s; integral type required", desc, t)
    }
  } yield y
  private def unsigned(eM: M[Expr], desc: String) (implicit pos: Pos): M[(Expr, UnsignedType)] = for {
    e <- eM
    y <- e match {
      case HasType(t: UnsignedType) => Result(e, t)
      case HasType(t)  => Problem("%s is of type %s; unsigned integral type required", desc, t)
    }
  } yield y
  
  private def sameAddable(e1M: M[Expr], e2M: M[Expr], d1: String, d2: String) (implicit pos: Pos) = for {
    ((e1, t1), (e2, t2)) <- together(addable(e1M, d1), addable(e2M, d2))
    t <- if (t1 == t2) Result(e1, e2, t1)
         else Problem("left and right operands have differing types %s and %s", t1, t2)
  } yield t
  private def sameNumeric(e1M: M[Expr], e2M: M[Expr], d1: String, d2: String) (implicit pos: Pos) = for {
    ((e1, t1), (e2, t2)) <- together(numeric(e1M, d1), numeric(e2M, d2))
    t <- if (t1 == t2) Result(e1, e2, t1)
         else Problem("left and right operands have differing types %s and %s", t1, t2)
  } yield t
  private def sameIntegral(e1M: M[Expr], e2M: M[Expr], d1: String, d2: String) (implicit pos: Pos) = for {
    ((e1, t1), (e2, t2)) <- together(integral(e1M, d1), integral(e2M, d2))
    t <- if (t1 == t2) Result(e1, e2, t1)
         else Problem("left and right operands have differing types %s and %s", t1, t2)
  } yield t
  private def sameUnsigned(e1M: M[Expr], e2M: M[Expr], d1: String, d2: String) (implicit pos: Pos) = for {
    ((e1, t1), (e2, t2)) <- together(unsigned(e1M, d1), unsigned(e2M, d2))
    t <- if (t1 == t2) Result(e1, e2, t1)
         else Problem("left and right operands have differing types %s and %s", t1, t2)
  } yield t

  def plus (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr]
  def minus(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr]
  def times(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr]
  def div  (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr]
  def mod  (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr]
  def pos(eM: M[Expr]) (implicit pos: Pos): M[Expr]
  def neg(eM: M[Expr]) (implicit pos: Pos): M[Expr]
  
  def bitAnd   (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr]
  def bitAndNot(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr]
  def bitOr    (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr]
  def bitXor   (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr]
  def shiftL   (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr]
  def shiftR   (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr]
  def compl(eM: M[Expr]) (implicit pos: Pos): M[Expr]
}
