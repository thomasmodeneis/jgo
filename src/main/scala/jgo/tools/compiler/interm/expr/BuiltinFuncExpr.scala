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
 */
trait BuiltinFuncExpr extends Expr {
  final val typeOf = BuiltinFuncType
  
  private[expr] final def eval = throw new UnsupportedOperationException("cannot eval a built-in func")
  private[expr] final def evalUnder = eval
}

trait BuiltinRegularFuncExpr extends BuiltinFuncExpr {
  def invoke(args: List[Expr]): Expr
}

abstract class BuiltinTypeFuncExpr extends BuiltinFuncExpr {
  def typeInvoke(t: Type, args: List[Expr]): Expr
}
