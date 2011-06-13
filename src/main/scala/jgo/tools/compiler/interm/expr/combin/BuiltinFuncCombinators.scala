package jgo.tools.compiler
package interm
package expr
package combin

import types._
import instr._
import instr.TypeConversions._
import codeseq._

trait BuiltinFuncCombinators extends Combinators {
  abstract override def invoke(callee: Expr, args: List[Expr])(pos: Pos): Err[Expr] = callee match {
    case b: BuiltinRegularFuncExpr =>
      if (b.canInvoke(args)) result(b.invoke(args))
      else problem("built-in func %s not compatible with provided arguments", b.name)(pos)
    
    case _ => super.invoke(callee, args)(pos)
  }
  
  def typeInvoke(callee: Expr, t: Type, args: List[Expr])(pos: Pos): Err[Expr] = callee match {
    case b: BuiltinTypeFuncExpr =>
      if (b.canTypeInvoke(t, args)) result(b.typeInvoke(t, args))
      else problem("built-in func %s not compatible with provided arguments", b.name)(pos)
    
    case _ => problem("cannot type-invoke a non-built-in func")(pos)
  }
}
