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
  def StringConstant(value: String)  (implicit pos: Pos): M[Constant] = new StringConst(value)
  def BoolConstant  (value: Boolean) (implicit pos: Pos): M[Constant] = new BoolConst(value)
  
  def IntConstant(value: BigInt) (implicit pos: Pos): M[Constant] =
    if (value.isValidInt) new IntConst(value.intValue)
    else Problem("%s is out of range", value)
  
  def FloatConstant(value: BigDecimal) (implicit pos: Pos): M[Constant] =
    if (!value.doubleValue.isInfinite) new FloatConst(value.doubleValue)
    else Problem("%s is out of range", value)
}
