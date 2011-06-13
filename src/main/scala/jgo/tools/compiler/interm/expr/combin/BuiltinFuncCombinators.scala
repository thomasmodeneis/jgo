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
    case b: BuiltinRegularFuncExpr => b.invoke(args)(pos)
    case _ => super.invoke(callee, args)(pos)
  }
  
  def typeInvoke(callee: Expr, t: Type, args: List[Expr])(pos: Pos): Err[Expr] = callee match {
    case b: BuiltinTypeFuncExpr => b.typeInvoke(t, args)(pos)
    case _ => problem("cannot type-invoke a non-built-in func")(pos)
  }
}
