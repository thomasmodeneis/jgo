package jgo.compiler
package interm
package expr

import message._
import message.Messaged._

import types._
import instr._
import instr.TypeConversions._
import codeseq._

import Utils._

private trait BasicCombinators extends Combinators with TypeChecks {
  def plus(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    (e1, e2) <- together(e1M, e2M)
    ut <- sameAddable(e1, e2, "left operand", "right operand")
  } yield ut match {
    case StringType     => BasicExpr(e1.eval |+| e2.eval |+| StrAdd, e1.t)
    case t: NumericType => BasicExpr(e1.eval |+| e2.eval |+| Add(t), e1.t)
  }
    
  def minus(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    (e1, e2) <- together(e1M, e2M)
    ut <- sameNumeric(e1, e2, "left operand", "right operand")
  } yield BasicExpr(e1.eval |+| e2.eval |+| Sub(ut), e1.t)
  
  def times(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    (e1, e2) <- together(e1M, e2M)
    ut <- sameNumeric(e1, e2, "left operand", "right operand")
  } yield BasicExpr(e1.eval |+| e2.eval |+| Mul(ut), e1.t)
  
  def div(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    (e1, e2) <- together(e1M, e2M)
    ut <- sameNumeric(e1, e2, "left operand", "right operand")
  } yield BasicExpr(e1.eval |+| e2.eval |+| Div(ut), e1.t)
  
  def mod(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    (e1, e2) <- together(e1M, e2M)
    ut <- sameIntegral(e1, e2, "left operand", "right operand")
  } yield BasicExpr(e1.eval |+| e2.eval |+| Mod(ut), e1.t)
  
  def pos(eM: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    e <- eM
    _ <- numeric(e, "operand of unary +")
  } yield e
  
  def neg(eM: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    e  <- eM
    ut <- numeric(e, "operand of unary -")
  } yield BasicExpr(e.eval |+| Neg(ut), e.t)
  
  
  def bitAnd(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    (e1, e2) <- together(e1M, e2M)
    ut <- sameIntegral(e1, e2, "left operand", "right operand")
  } yield BasicExpr(e1.eval |+| e2.eval |+| BitwiseAnd(ut), e1.t)
  
  def bitAndNot(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    (e1, e2) <- together(e1M, e2M)
    ut <- sameIntegral(e1, e2, "left operand", "right operand")
  } yield BasicExpr(e1.eval |+| e2.eval |+| BitwiseAndNot(ut), e1.t)
  
  def bitOr(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    (e1, e2) <- together(e1M, e2M)
    ut <- sameIntegral(e1, e2, "left operand", "right operand")
  } yield BasicExpr(e1.eval |+| e2.eval |+| BitwiseOr(ut), e1.t)
  
  def bitXor(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    (e1, e2) <- together(e1M, e2M)
    ut <- sameIntegral(e1, e2, "left operand", "right operand")
  } yield BasicExpr(e1.eval |+| e2.eval |+| BitwiseXor(ut), e1.t)
  
  def shiftL(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    (e1, e2)   <- together(e1M, e2M)
    (ut1, ut2) <- together(integral(e1, "left operand of shift"),
                           unsigned(e2, "right operand of shift"))
  } yield BasicExpr(e1.eval |+| e2.eval |+| ShiftL(ut1, ut2), e1.t)
  
  def shiftR(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    (e1, e2)   <- together(e1M, e2M)
    (ut1, ut2) <- together(integral(e1, "left operand of shift"),
                           unsigned(e2, "right operand of shift"))
  } yield BasicExpr(e1.eval |+| e2.eval |+| ShiftR(ut1, ut2), e1.t)
  
  def bitCompl(eM: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    e <- eM
    ut <- integral(e, "operand of bitwise complement")
  } yield BasicExpr(e.eval |+| BitwiseCompl(ut), e.t)
  
  
  def chanRecv(chanM: M[Expr]) (implicit pos: Pos): M[Expr] =
    for (chan <- chanM; t <- recvChanT(chan))
    yield BasicExpr(chan.eval |+| ChanRecv, t)
  
  def chanSend(chanM: M[Expr], eM: M[Expr]) (implicit pos: Pos): M[CodeBuilder] = for {
    chan <- chanM
    t <- sendChanT(chan)
    e <- eM
    _ <- if (t <<= e.t) Result(())
         else Problem("type %s of right operand of channel send not assignable to element type %s of left operand",
                      e.t, t)
  } yield chan.eval |+| e.eval |+| ChanSend
  
  
  private def checkCall(callee: Expr, args: List[Expr]) (implicit pos: Pos): M[Type] = callee match {
    case HasType(FuncType(_, List(res0, res1, _*), _)) => Problem("polyadic results not currently supported")
    case HasType(FuncType(params, results, true))      => Problem("variadic calls not yet supported")
    case HasType(FuncType(params, results, false)) =>
      if (params.length != args.length)
        Problem("number (%d) of arguments passed unequal to number (%d) required",
                args.length, params.length)
      else {
        for (((param, HasType(arg)), index) <- (params zip args).zipWithIndex) if (!(param <<= arg))
          return Problem(
            "%s argument has type %s, which is not assignable to corresponding parameter type %s",
            ordinal(index + 1), arg, param)
        Result(results.headOption getOrElse UnitType)
      }
    
    case _ => Problem("callee has type %s; function type required", callee.t)
  }
  def invoke(calleeM: M[Expr], argsM: M[List[Expr]]) (implicit pos: Pos): M[Expr] = for {
    callee <- calleeM
    args <- argsM
    resultT <- checkCall(callee, args)
  } yield callee.mkCall(args, resultT)
  
  
  def typeAssert(eM: M[Expr], tM: M[Type]) (implicit pos: Pos): M[Expr] = for {
    e <- eM
    t <- tM
  } yield BasicExpr(e.eval |+| TypeAssert(t), t)
}
