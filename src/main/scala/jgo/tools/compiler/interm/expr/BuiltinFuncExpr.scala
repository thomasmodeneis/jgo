package jgo.tools.compiler
package interm
package expr

import types._

/**
 * An expression referring to a built-in func, like `len` or `make`.
 * Such an item is not actually a func (it doesn't have function type),
 * so the term "built-in func" is something of a misnomer.
 * 
 * Implementations of built-in funcs are found in `expr.bfunc`.
 */
trait BuiltinFuncExpr extends Expr {
  final val typeOf = BuiltinFuncType
  
  private[expr] final def eval = throw new UnsupportedOperationException("cannot eval a built-in func")
  private[expr] final def evalUnder = eval
  
  def name: String
}

trait BuiltinRegularFuncExpr extends BuiltinFuncExpr {
  def invoke(args: List[Expr])(pos: Pos): Err[Expr]
}

abstract class BuiltinTypeFuncExpr extends BuiltinFuncExpr {
  def typeInvoke(t: Type, args: List[Expr])(pos: Pos): Err[Expr]
}
