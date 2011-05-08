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

trait BasicCombinators extends Combinators with TypeChecks {
  def plus(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] =
    for ((e1a, e2a, at) <- sameAddable(e1, e2))
    yield at match {
      case StringType     => BasicExpr(e1a.eval |+| e2a.eval |+| StrAdd, e1.t)
      case t: NumericType => BasicExpr(e1a.eval |+| e2a.eval |+| Add(t), e1.t)
    }
    
  def minus(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] =
    for ((e1n, e2n, nt) <- sameNumeric(e1, e2))
    yield BasicExpr(e1n.eval |+| e2n.eval |+| Sub(nt), e1n.t)
  
  def times(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] = 
    for ((e1n, e2n, nt) <- sameNumeric(e1, e2))
    yield BasicExpr(e1n.eval |+| e2n.eval |+| Mul(nt), e1.t)
  
  def div(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] = 
    for ((e1n, e2n, nt) <- sameNumeric(e1, e2))
    yield BasicExpr(e1n.eval |+| e2n.eval |+| Div(nt), e1.t)
  
  def mod(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] = 
    for ((e1i, e2i, it) <- sameIntegral(e1, e2))
    yield BasicExpr(e1i.eval |+| e2i.eval |+| Mod(it), e1.t)
  
  def pos(e: Expr) (implicit pos: Pos): M[Expr] =
    for ((en, _) <- numeric(e, "operand of unary +"))
    yield en
  
  def neg(e: Expr) (implicit pos: Pos): M[Expr] =
    for ((en, nt) <- numeric(e, "operand of unary -"))
    yield BasicExpr(en.eval |+| Neg(nt), e.t)
  
  
  def bitAnd(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] =
    for ((e1i, e2i, it) <- sameIntegral(e1, e2))
    yield BasicExpr(e1i.eval |+| e2i.eval |+| BitwiseAnd(it), e1.t)
  
  def bitAndNot(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] =
    for ((e1i, e2i, it) <- sameIntegral(e1, e2))
    yield BasicExpr(e1i.eval |+| e2i.eval |+| BitwiseAndNot(it), e1.t)
  
  def bitOr(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] =
    for ((e1i, e2i, it) <- sameIntegral(e1, e2))
    yield BasicExpr(e1i.eval |+| e2i.eval |+| BitwiseOr(it), e1.t)
  
  def bitXor(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] =
    for ((e1i, e2i, it) <- sameIntegral(e1, e2))
    yield BasicExpr(e1i.eval |+| e2i.eval |+| BitwiseXor(it), e1.t)
  
  def shiftL(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] = for {
    ((e1i, it1), (e2u, ut2)) <- together(integral(e1, "left operand of shift"),
                                         unsigned(e2, "right operand of shift"))
  } yield BasicExpr(e1i.eval |+| e2u.eval |+| ShiftL(it1, ut2), e1.t)
  
  def shiftR(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr] = for {
    ((e1i, it1), (e2u, ut2)) <- together(integral(e1, "left operand of shift"),
                                         unsigned(e2, "right operand of shift"))
  } yield BasicExpr(e1i.eval |+| e2u.eval |+| ShiftR(it1, ut2), e1.t)
  
  def bitCompl(e: Expr) (implicit pos: Pos): M[Expr] =
    for ((ei, it) <- integral(e, "operand of bitwise complement"))
    yield BasicExpr(ei.eval |+| BitwiseCompl(it), e.t)
  
  
  def chanRecv(ch: Expr) (implicit pos: Pos): M[Expr] =
    for ((chan, elemT) <- recvChanT(ch, "operand of channel receive"))
    yield BasicExpr(chan.eval |+| ChanRecv, elemT)
  
  def chanSend(ch: Expr, e: Expr) (implicit pos: Pos): M[CodeBuilder] = for {
    (chan, elemT) <- sendChanT(ch, "left operand of channel send")
    _ <- if (elemT <<= e.t) Result(())
         else Problem("type %s of right operand of channel send not assignable to element type %s of left operand",
                      e.t, elemT)
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
