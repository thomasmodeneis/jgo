package jgo.tools.compiler
package interm
package expr
package bfunc

import types._

/**
 * A built-in func, such as `len` or `make`.
 * 
 * Such an item is not actually a func (it doesn't have function type),
 * so the term "built-in func" is something of a misnomer. Moreover,
 * built-in funcs are not Exprs; they cannot be used as rvalues, etc.
 * 
 * Built-in funcs are implemented in `interm.expr` since they require
 * privileged access to the expression framework.
 */
trait BuiltinFunc extends Typed {
  final val typeOf = BuiltinFuncType
  def name: String
}

trait BuiltinRegularFunc extends BuiltinFunc {
  def invoke(args: List[Expr])(pos: Pos): Err[Expr]
}

abstract class BuiltinTypeFunc extends BuiltinFunc {
  def typeInvoke(t: Type, args: List[Expr])(pos: Pos): Err[Expr]
}
