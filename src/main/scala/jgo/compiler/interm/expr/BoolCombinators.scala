package jgo.compiler
package interm
package expr

import message._
import message.Messaged._

import types._
import instr._
import instr.TypeConversions._
import codeseq._

import Utils._

private trait BoolCombinators extends Combinators with TypeChecks {
  def and(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos) = for {
    (e1, e2) <- together(e1M, e2M)
    (b1, b2) <- sameBoolExpr(e1, e2, "left operand", "right operand")
  } yield new And(b1, b2)
  
  def or(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos) = for {
    (e1, e2) <- together(e1M, e2M)
    (b1, b2) <- sameBoolExpr(e1, e2, "left operand", "right operand")
  } yield new Or(b1, b2)
  
  def not(eM: M[Expr]) (implicit pos: Pos) = for {
    e <- eM
    b <- boolExpr(e, "operand")
  } yield new Not(b)
  
  
  def compEq(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    (e1, e2) <- together(e1M, e2M)
    t <- sameType(e1, e2, "left operand", "right operand")
  } yield t match {
    case BoolType => BoolEquals(e1, e2)
    case nt: NumericType => NumEquals(e1, e2, nt)
    case _ => ObjEquals(e1, e2)
  }
  
  def compNe(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    (e1, e2) <- together(e1M, e2M)
    t <- sameType(e1, e2, "left operand", "right operand")
  } yield t match {
    case BoolType => BoolNotEquals(e1, e2)
    case nt: NumericType => NumNotEquals(e1, e2, nt)
    case _ => ObjNotEquals(e1, e2)
  }
  
  
  def compLt(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos) = for {
    (e1, e2) <- together(e1M, e2M)
    nt <- sameNumeric(e1, e2, "left operand", "right operand")
  } yield LessThan(e1, e2, nt)
  
  def compLeq(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos) = for {
    (e1, e2) <- together(e1M, e2M)
    nt <- sameNumeric(e1, e2, "left operand", "right operand")
  } yield LessEquals(e1, e2, nt)
  
  def compGt(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos) = for {
    (e1, e2) <- together(e1M, e2M)
    nt <- sameNumeric(e1, e2, "left operand", "right operand")
  } yield GreaterThan(e1, e2, nt)
  
  def compGeq(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos) = for {
    (e1, e2) <- together(e1M, e2M)
    nt <- sameNumeric(e1, e2, "left operand", "right operand")
  } yield GreaterEquals(e1, e2, nt)
  
}
