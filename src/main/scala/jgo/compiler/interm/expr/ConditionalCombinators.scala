package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

import Utils._

trait ConditionalCombinators extends Combinators with TypeChecks {
  def conditional(e: Expr) (implicit pos: Pos): M[ConditionalExpr] =
    condExpr(e, "expression")
  
  
  def and(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] =
    for ((b1, b2) <- sameCondExpr(e1, e2))
    yield new And(b1, b2, e1.typeOf)
  
  def or(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] =
    for ((b1, b2) <- sameCondExpr(e1, e2))
    yield new Or(b1, b2, e1.typeOf)
  
  def not(e: Expr) (implicit pos: Pos): M[Expr] =
    for (b <- condExpr(e, "operand"))
    yield new Not(b, e.typeOf)
  
  
  def compEq(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] =
    for (t <- sameType(e1, e2))
    yield t.underlying match {
      case BoolType => BoolEquals(e1, e2)
      case nt: NumericType => NumEquals(e1, e2, nt)
      case _ => ObjEquals(e1, e2)
    }
  
  def compNe(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] =
    for (t <- sameType(e1, e2))
    yield t.underlying match {
      case BoolType        => BoolNotEquals(e1, e2)
      case nt: NumericType => NumNotEquals(e1, e2, nt)
      case _ => ObjNotEquals(e1, e2)
    }
  
  //TODO:  Add support for comparing strings.  Low priority.
  def compLt(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] =
    for ((e1n, e2n, nt) <- sameNumeric(e1, e2))
    yield LessThan(e1n, e2n, nt)
  
  def compLeq(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] =
    for ((e1n, e2n, nt) <- sameNumeric(e1, e2))
    yield LessEquals(e1n, e2n, nt)
  
  def compGt(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] =
    for ((e1n, e2n, nt) <- sameNumeric(e1, e2))
    yield GreaterThan(e1n, e2n, nt)
  
  def compGeq(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] =
    for ((e1n, e2n, nt) <- sameNumeric(e1, e2))
    yield GreaterEquals(e1n, e2n, nt)
}
