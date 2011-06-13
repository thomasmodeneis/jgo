package jgo.tools.compiler
package interm
package expr

import types._
import symbol._
import instr._
import instr.TypeConversions._
import codeseq._

/**
 * An expression referring to a built-in func, like `len` or `make`.
 * Such an item is not actually a func (it doesn't have function type),
 * so we call it a ''bfunc''.
 */
trait BuiltinFuncExpr extends Expr {
  final val typeOf = BuiltinFuncType
  
  private[expr] final def eval = throw new UnsupportedOperationException("cannot eval a built-in func")
  private[expr] final def evalUnder = eval
  
  def name: String
}

trait BuiltinRegularFuncExpr extends BuiltinFuncExpr {
  def canInvoke(args: List[Expr]): Boolean
  def invoke(args: List[Expr]): Expr
}

abstract class BuiltinTypeFuncExpr extends BuiltinFuncExpr {
  def canTypeInvoke(t: Type, args: List[Expr]): Boolean
  def typeInvoke(t: Type, args: List[Expr]): Expr
}
