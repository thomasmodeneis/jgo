package jgo.tools.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import symbol._
import codeseq._

trait ConversionCombinators extends Combinators with TypeChecks {
  def assignableTo(e: Expr, t: Type)(pos: Pos) =
    if (t <<= e.typeOf) result(e)
    else problem("expression of type %s not assignable to target type %s", e.typeOf, t)(pos)
  
  protected def convertForAssign(e: Expr, t: Type, desc: String)(pos: Pos): Err[Expr] =
    if (t <<= e.typeOf)
      if (t != NilType)
        result(UnderlyingExpr(e.evalUnder, t))
      else
        result(EvalExpr(PushNil, t))
    else
      problem("%s of type %s not assignable to target type %s", desc, e.typeOf, t)(pos)
  
  def convert(e: Expr, t: Type)(pos: Pos) =
    throw new UnsupportedOperationException
}
