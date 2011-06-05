package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

import Utils._

trait ConditionalCombinators extends Combinators with TypeChecks {
  def conditional(e: Expr)(pos: Pos): Err[ConditionalExpr] =
    condExpr(e, "expression")(pos)
  
  
  def and(e1: Expr, e2: Expr)(pos: Pos): Err[Expr] =
    for ((b1, b2) <- sameCondExpr(e1, e2)(pos))
    yield new And(b1, b2, e1.typeOf)
  
  def or(e1: Expr, e2: Expr)(pos: Pos): Err[Expr] =
    for ((b1, b2) <- sameCondExpr(e1, e2)(pos))
    yield new Or(b1, b2, e1.typeOf)
  
  def not(e: Expr)(pos: Pos): Err[Expr] =
    for (b <- condExpr(e, "operand")(pos))
    yield new Not(b, e.typeOf)
  
  
  def compEq(e1: Expr, e2: Expr)(pos: Pos): Err[Expr] =
    for ((e1, e2, t) <- sameType(e1, e2)(pos))
    yield t.underlying match {
      case BoolType        => BoolEquals(e1, e2)
      case nt: NumericType => NumEquals(e1, e2, nt)
      case _               => ObjEquals(e1, e2)
    }
  
  def compNe(e1: Expr, e2: Expr)(pos: Pos): Err[Expr] =
    for ((e1s, e2s, t) <- sameType(e1, e2)(pos))
    yield t.underlying match {
      case BoolType        => BoolNotEquals(e1s, e2s)
      case nt: NumericType => NumNotEquals(e1s, e2s, nt)
      case _               => ObjNotEquals(e1s, e2s)
    }
  
  //TODO:  Add support for comparing strings.  Low priority.
  def compLt(e1: Expr, e2: Expr)(pos: Pos): Err[Expr] =
    for ((e1n, e2n, nt) <- sameNumeric(e1, e2)(pos))
    yield LessThan(e1n, e2n, nt)
  
  def compLeq(e1: Expr, e2: Expr)(pos: Pos): Err[Expr] =
    for ((e1n, e2n, nt) <- sameNumeric(e1, e2)(pos))
    yield LessEquals(e1n, e2n, nt)
  
  def compGt(e1: Expr, e2: Expr)(pos: Pos): Err[Expr] =
    for ((e1n, e2n, nt) <- sameNumeric(e1, e2)(pos))
    yield GreaterThan(e1n, e2n, nt)
  
  def compGeq(e1: Expr, e2: Expr)(pos: Pos): Err[Expr] =
    for ((e1n, e2n, nt) <- sameNumeric(e1, e2)(pos))
    yield GreaterEquals(e1n, e2n, nt)
}
