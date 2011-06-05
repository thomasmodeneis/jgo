package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

import Utils._

trait BasicCombinators extends Combinators with TypeChecks {
  def plus(e1: Expr, e2: Expr)(pos: Pos): M[Expr] =
    for ((e1a, e2a, at) <- sameAddable(e1, e2)(pos))
    yield at match {
      case StringType     => UnderlyingExpr(e1a.evalUnder |+| e2a.evalUnder |+| StrAdd, e1.typeOf)
      case t: NumericType => UnderlyingExpr(e1a.evalUnder |+| e2a.evalUnder |+| Add(t), e1.typeOf)
    }
    
  def minus(e1: Expr, e2: Expr)(pos: Pos): M[Expr] =
    for ((e1n, e2n, nt) <- sameNumeric(e1, e2)(pos))
    yield UnderlyingExpr(e1n.evalUnder |+| e2n.evalUnder |+| Sub(nt), e1n.typeOf)
  
  def times(e1: Expr, e2: Expr)(pos: Pos): M[Expr] = 
    for ((e1n, e2n, nt) <- sameNumeric(e1, e2)(pos))
    yield UnderlyingExpr(e1n.evalUnder |+| e2n.evalUnder |+| Mul(nt), e1.typeOf)
  
  def div(e1: Expr, e2: Expr)(pos: Pos): M[Expr] = 
    for ((e1n, e2n, nt) <- sameNumeric(e1, e2)(pos))
    yield UnderlyingExpr(e1n.evalUnder |+| e2n.evalUnder |+| Div(nt), e1.typeOf)
  
  def mod(e1: Expr, e2: Expr)(pos: Pos): M[Expr] = 
    for ((e1i, e2i, it) <- sameIntegral(e1, e2)(pos))
    yield UnderlyingExpr(e1i.evalUnder |+| e2i.evalUnder |+| Mod(it), e1.typeOf)
  
  def positive(e: Expr)(pos: Pos): M[Expr] =
    for (_ <- numeric(e, "operand of unary +")(pos))
    yield e
  
  def negative(e: Expr)(pos: Pos): M[Expr] =
    for (nt <- numeric(e, "operand of unary -")(pos))
    yield UnderlyingExpr(e.evalUnder |+| Neg(nt), e.typeOf)
  
  
  def bitAnd(e1: Expr, e2: Expr)(pos: Pos): M[Expr] =
    for ((e1i, e2i, it) <- sameIntegral(e1, e2)(pos))
    yield UnderlyingExpr(e1i.evalUnder |+| e2i.evalUnder |+| BitwiseAnd(it), e1.typeOf)
  
  def bitAndNot(e1: Expr, e2: Expr)(pos: Pos): M[Expr] =
    for ((e1i, e2i, it) <- sameIntegral(e1, e2)(pos))
    yield UnderlyingExpr(e1i.evalUnder |+| e2i.evalUnder |+| BitwiseAndNot(it), e1.typeOf)
  
  def bitOr(e1: Expr, e2: Expr)(pos: Pos): M[Expr] =
    for ((e1i, e2i, it) <- sameIntegral(e1, e2)(pos))
    yield UnderlyingExpr(e1i.evalUnder |+| e2i.evalUnder |+| BitwiseOr(it), e1.typeOf)
  
  def bitXor(e1: Expr, e2: Expr)(pos: Pos): M[Expr] =
    for ((e1i, e2i, it) <- sameIntegral(e1, e2)(pos))
    yield UnderlyingExpr(e1i.evalUnder |+| e2i.evalUnder |+| BitwiseXor(it), e1.typeOf)
  
  def shiftL(e1: Expr, e2: Expr)(pos: Pos): M[Expr] = for {
    (it1, ut2) <- (integral(e1, "left operand of shift")(pos),
                   unsigned(e2, "right operand of shift")(pos))
  } yield UnderlyingExpr(e1.evalUnder |+| e2.evalUnder |+| ShiftL(it1, ut2), e1.typeOf)
  
  def shiftR(e1: Expr, e2: Expr)(pos: Pos): M[Expr] = for {
    (it1, ut2) <- (integral(e1, "left operand of shift")(pos),
                   unsigned(e2, "right operand of shift")(pos))
  } yield UnderlyingExpr(e1.evalUnder |+| e2.evalUnder |+| ShiftR(it1, ut2), e1.typeOf)
  
  def bitCompl(e: Expr)(pos: Pos): M[Expr] =
    for (it <- integral(e, "operand of bitwise complement")(pos))
    yield UnderlyingExpr(e.evalUnder |+| BitwiseCompl(it), e.typeOf)
  
  
  def chanRecv(ch: Expr)(pos: Pos): M[Expr] =
    for (elemT <- recvChanT(ch, "operand of channel receive")(pos))
    yield EvalExpr(ch.evalUnder |+| ChanRecv, elemT)
  
  def chanSend(ch: Expr, e: Expr)(pos: Pos): M[CodeBuilder] = for {
    elemT <- sendChanT(ch, "left operand of channel send")(pos)
    _ <- if (elemT <<= e.typeOf) Result(())
         else Problem("type %s of right operand of channel send not assignable to element type %s of left operand",
                      e.typeOf, elemT)(pos)
  } yield ch.evalUnder |+| e.eval |+| ChanSend //TODO: Add code that converts e to the appropriate type
  
  
  private def checkCall(callee: Expr, args: List[Expr])(pos: Pos): M[Type] = callee match {
    case HasType(FuncType(_, List(res0, res1, _*), _)) => Problem("polyadic results not currently supported")(pos)
    case HasType(FuncType(params, results, true))      => Problem("variadic calls not yet supported")(pos)
    case HasType(FuncType(params, results, false)) =>
      if (params.length != args.length)
        Problem("number (%d) of arguments passed unequal to number (%d) required",
                args.length, params.length)(pos)
      else {
        for (((param, HasType(arg)), index) <- (params zip args).zipWithIndex) if (!(param <<= arg))
          return Problem(
            "%s argument has type %s, which is not assignable to corresponding parameter type %s",
            ordinal(index + 1), arg, param)(pos)
        Result(results.headOption getOrElse UnitType)
      }
    
    case _ => Problem("callee has type %s; function type required", callee.typeOf)(pos)
  }
  
  def invoke(callee: Expr, args: List[Expr])(pos: Pos): M[Expr] = for {
    resultT <- checkCall(callee, args)(pos)
  } yield callee.mkCall(args, resultT)
  
  
  /** @todo bring in line with spec, which, I believe, permits type asserts only on interface values */
  def typeAssert(e: Expr, t: Type)(pos: Pos): M[Expr] =
    Result(EvalExpr(e.eval |+| TypeAssert(t), t))
}
