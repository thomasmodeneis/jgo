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
  def IntConstant(value: BigInt): M[ConstExpr] =
    new IntConst(value.intValue)
  
  def FloatConstant(value: BigDecimal): M[ConstExpr] =
    new FloatConst(value.doubleValue)
  
  def varLval(v: Variable): Expr = VarLval(v) //should these be in Combinators?
  
  def funcExpr(f: Function): Expr = FuncExpr(f)
  
  implicit def mappedToExpr[A](ls: List[A])(implicit ev: A => Expr): List[Expr] =
    ls map ev
}
