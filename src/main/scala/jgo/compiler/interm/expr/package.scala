package jgo.compiler
package interm

import message._

import types._
import symbol._
import instr._
import instr.TypeConversions._
import codeseq._

import scala.util.parsing.input.Position

package object expr {
  def IntConstant(value: BigInt) (implicit pos: Pos): M[ConstExpr] =
    if (value.isValidInt) new IntConst(value.intValue)
    else Problem("%s is out of range", value)
  
  def FloatConstant(value: BigDecimal) (implicit pos: Pos): M[ConstExpr] =
    if (!value.doubleValue.isInfinite) new FloatConst(value.doubleValue)
    else Problem("%s is out of range", value)
  
  def varLval(v: Variable): Expr = VarLval(v) //should these be in Combinators?
  
  def funcExpr(f: Function): Expr = FuncExpr(f)
  
  implicit def mappedToExpr[A](ls: List[A])(implicit ev: A => Expr): List[Expr] =
    ls map ev
}
