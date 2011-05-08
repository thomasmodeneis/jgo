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
  def plus(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] = for {
    ut <- sameAddable(e1, e2, "left operand", "right operand")
  } yield ut match {
    case StringType     => BasicExpr(e1.eval |+| e2.eval |+| StrAdd, e1.t)
    case t: NumericType => BasicExpr(e1.eval |+| e2.eval |+| Add(t), e1.t)
  }
    
  def minus(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] = for {
    ut <- sameNumeric(e1, e2, "left operand", "right operand")
  } yield BasicExpr(e1.eval |+| e2.eval |+| Sub(ut), e1.t)
  
  def times(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] = for {
    ut <- sameNumeric(e1, e2, "left operand", "right operand")
  } yield BasicExpr(e1.eval |+| e2.eval |+| Mul(ut), e1.t)
  
  def div(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] = for {
    ut <- sameNumeric(e1, e2, "left operand", "right operand")
  } yield BasicExpr(e1.eval |+| e2.eval |+| Div(ut), e1.t)
  
  def mod(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] = for {
    ut <- sameIntegral(e1, e2, "left operand", "right operand")
  } yield BasicExpr(e1.eval |+| e2.eval |+| Mod(ut), e1.t)
  
  def pos(e: Expr) (implicit pos: Pos): M[Expr] = for {
    _ <- numeric(e, "operand of unary +")
  } yield e
  
  def neg(e: Expr) (implicit pos: Pos): M[Expr] = for {
    ut <- numeric(e, "operand of unary -")
  } yield BasicExpr(e.eval |+| Neg(ut), e.t)
  
  
  def bitAnd(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] = for {
    ut <- sameIntegral(e1, e2, "left operand", "right operand")
  } yield BasicExpr(e1.eval |+| e2.eval |+| BitwiseAnd(ut), e1.t)
  
  def bitAndNot(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] = for {
    ut <- sameIntegral(e1, e2, "left operand", "right operand")
  } yield BasicExpr(e1.eval |+| e2.eval |+| BitwiseAndNot(ut), e1.t)
  
  def bitOr(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] = for {
    ut <- sameIntegral(e1, e2, "left operand", "right operand")
  } yield BasicExpr(e1.eval |+| e2.eval |+| BitwiseOr(ut), e1.t)
  
  def bitXor(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] = for {
    ut <- sameIntegral(e1, e2, "left operand", "right operand")
  } yield BasicExpr(e1.eval |+| e2.eval |+| BitwiseXor(ut), e1.t)
  
  def shiftL(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] = for {
    (ut1, ut2) <- together(integral(e1, "left operand of shift"),
                           unsigned(e2, "right operand of shift"))
  } yield BasicExpr(e1.eval |+| e2.eval |+| ShiftL(ut1, ut2), e1.t)
  
  def shiftR(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] = for {
    (ut1, ut2) <- together(integral(e1, "left operand of shift"),
                           unsigned(e2, "right operand of shift"))
  } yield BasicExpr(e1.eval |+| e2.eval |+| ShiftR(ut1, ut2), e1.t)
  
  def bitCompl(e: Expr) (implicit pos: Pos): M[Expr] = for {
    ut <- integral(e, "operand of bitwise complement")
  } yield BasicExpr(e.eval |+| BitwiseCompl(ut), e.t)
  
  
  def chanRecv(chan: Expr) (implicit pos: Pos): M[Expr] =
    for (t <- recvChanT(chan, "operand of channel receive"))
    yield BasicExpr(chan.eval |+| ChanRecv, t)
  
  def chanSend(chan: Expr, e: Expr) (implicit pos: Pos): M[CodeBuilder] = for {
    t <- sendChanT(chan, "left operand of channel send")
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
  def invoke(callee: Expr, args: List[Expr]) (implicit pos: Pos): M[Expr] = for {
    resultT <- checkCall(callee, args)
  } yield callee.mkCall(args, resultT)
  
  
  def typeAssert(e: Expr, t: Type) (implicit pos: Pos): M[Expr] =
    BasicExpr(e.eval |+| TypeAssert(t), t)
}
