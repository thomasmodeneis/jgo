package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import symbol._
import codeseq._

trait ConversionCombinators extends Combinators with TypeChecks {
  def assignableTo(e: Expr, targetType: Type) (implicit pos: Pos) =
    if (targetType <<= e.typeOf) Result(e)
    else Problem("expression not assignable to target type %s", targetType)
  
  def convertForAssign(e: Expr, t: Type) (implicit pos: Pos) =
    if (t <<= e.typeOf)
      if (t != NilType)
        Result(UnderlyingExpr(e.evalUnder, t))
      else
        Result(EvalExpr(PushNil, t))
    else
      Problem("expression of type %s not assignable to type %s", e.typeOf, t)
  
  def convert(e: Expr, t: Type) (implicit pos: Pos) =
    throw new UnsupportedOperationException
}
