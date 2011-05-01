package jgo.compiler
package interm
package expr

import message._
import message.Messaged._

import types._
import instr._
import instr.TypeConversions._
import codeseq._

import scala.util.parsing.input.Position

private trait BasicCombinators extends Combinators {
  private def addable(eM: M[Expr], desc: String) (implicit pos: Pos): M[(Expr, AddableType)] = for {
    e <- eM
    y <- e match {
      case HasType(ut: AddableType) => Result(e, ut)
      case _ => Problem("%s is of type %s; numeric or string type required", desc, e.t)
    }
  } yield y
  private def numeric(eM: M[Expr], desc: String) (implicit pos: Pos): M[(Expr, NumericType)] = for {
    e <- eM
    y <- e match {
      case HasType(ut: NumericType) => Result(e, ut)
      case _ => Problem("%s is of type %s; numeric type required", desc, e.t)
    }
  } yield y
  private def integral(eM: M[Expr], desc: String) (implicit pos: Pos): M[(Expr, IntegralType)] = for {
    e <- eM
    y <- e match {
      case HasType(ut: IntegralType) => Result(e, ut)
      case _ => Problem("%s is of type %s; integral type required", desc, e.t)
    }
  } yield y
  private def unsigned(eM: M[Expr], desc: String) (implicit pos: Pos): M[(Expr, UnsignedType)] = for {
    e <- eM
    y <- e match {
      case HasType(ut: UnsignedType) => Result(e, ut)
      case _ => Problem("%s is of type %s; unsigned integral type required", desc, e.t)
    }
  } yield y
  
  private def sameAddable(e1M: M[Expr], e2M: M[Expr], d1: String, d2: String) (implicit pos: Pos) = for {
    ((e1, ut1), (e2, ut2)) <- together(addable(e1M, d1), addable(e2M, d2))
    t <- if (e1.t == e2.t) Result(e1, e2, ut1)
         else Problem("left and right operands have differing types %s and %s", e1.t, e2.t)
  } yield t
  private def sameNumeric(e1M: M[Expr], e2M: M[Expr], d1: String, d2: String) (implicit pos: Pos) = for {
    ((e1, ut1), (e2, ut2)) <- together(numeric(e1M, d1), numeric(e2M, d2))
    t <- if (e1.t == e2.t) Result(e1, e2, ut1)
         else Problem("left and right operands have differing types %s and %s", e1.t, e2.t)
  } yield t
  private def sameIntegral(e1M: M[Expr], e2M: M[Expr], d1: String, d2: String) (implicit pos: Pos) = for {
    ((e1, ut1), (e2, ut2)) <- together(integral(e1M, d1), integral(e2M, d2))
    t <- if (e1.t == e2.t) Result(e1, e2, ut1)
         else Problem("left and right operands have differing types %s and %s", e1.t, e2.t)
  } yield t
  private def sameUnsigned(e1M: M[Expr], e2M: M[Expr], d1: String, d2: String) (implicit pos: Pos) = for {
    ((e1, ut1), (e2, ut2)) <- together(unsigned(e1M, d1), unsigned(e2M, d2))
    t <- if (e1.t == e2.t) Result(e1, e2, ut1)
         else Problem("left and right operands have differing types %s and %s", e1.t, e2.t)
  } yield t
  
  
  def plus(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] =
    for ((e1, e2, ut) <- sameAddable(e1M, e2M, "left operand", "right operand")) yield ut match {
      case StringType     => BasicExpr(e1.eval |+| e2.eval |+| StrAdd, e1.t)
      case t: NumericType => BasicExpr(e1.eval |+| e2.eval |+| Add(t), e1.t)
    }
    
  def minus(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] =
    for ((e1, e2, ut) <- sameNumeric(e1M, e2M, "left operand", "right operand"))
    yield BasicExpr(e1.eval |+| e2.eval |+| Sub(ut), e1.t)
  
  def times(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] =
    for ((e1, e2, ut) <- sameNumeric(e1M, e2M, "left operand", "right operand"))
    yield BasicExpr(e1.eval |+| e2.eval |+| Mul(ut), e1.t)
  
  def div(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] =
    for ((e1, e2, ut) <- sameNumeric(e1M, e2M, "left operand", "right operand"))
    yield BasicExpr(e1.eval |+| e2.eval |+| Div(ut), e1.t)
  
  def mod(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] =
    for ((e1, e2, ut) <- sameIntegral(e1M, e2M, "left operand", "right operand"))
    yield BasicExpr(e1.eval |+| e2.eval |+| Mod(ut), e1.t)
  
  def pos(eM: M[Expr]) (implicit pos: Pos): M[Expr] =
    for ((e, _) <- numeric(eM, "operand of unary +")) yield e
  
  def neg(eM: M[Expr]) (implicit pos: Pos): M[Expr] =
    for ((e, ut) <- numeric(eM, "operand of unary -")) yield BasicExpr(e.eval |+| Neg(ut), e.t)
  
  
  def bitAnd(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] =
    for ((e1, e2, ut) <- sameIntegral(e1M, e2M, "left operand", "right operand"))
    yield BasicExpr(e1.eval |+| e2.eval |+| BitwiseAnd(ut), e1.t)
  
  def bitAndNot(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] =
    for ((e1, e2, ut) <- sameIntegral(e1M, e2M, "left operand", "right operand"))
    yield BasicExpr(e1.eval |+| e2.eval |+| BitwiseAndNot(ut), e1.t)
  
  def bitOr(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] =
    for ((e1, e2, ut) <- sameIntegral(e1M, e2M, "left operand", "right operand"))
    yield BasicExpr(e1.eval |+| e2.eval |+| BitwiseOr(ut), e1.t)
  
  def bitXor(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] =
    for ((e1, e2, ut) <- sameIntegral(e1M, e2M, "left operand", "right operand"))
    yield BasicExpr(e1.eval |+| e2.eval |+| BitwiseXor(ut), e1.t)
  
  def shiftL(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    ((e1, ut1), (e2, ut2)) <- together(integral(e1M, "left operand of shift"),
                                       unsigned(e2M, "right operand of shift"))
  } yield BasicExpr(e1.eval |+| e2.eval |+| ShiftL(ut1, ut2), e1.t)
  
  def shiftR(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    ((e1, ut1), (e2, ut2)) <- together(integral(e1M, "left operand of shift"),
                                       unsigned(e2M, "right operand of shift"))
  } yield BasicExpr(e1.eval |+| e2.eval |+| ShiftR(ut1, ut2), e1.t)
  
  def bitCompl(eM: M[Expr]) (implicit pos: Pos): M[Expr] =
    for ((e, ut) <- integral(eM, "operand of bitwise complement"))
    yield BasicExpr(e.eval |+| BitwiseCompl(ut), e.t)
  
  
  private def recvChanT(e: Expr) (implicit pos: Pos): M[Type] = e match {
    case HasType(RecvChanType(t)) => Result(t)
    case _ => Problem("operand of channel receive has type %s; receiving chan type required", e.t)
  }
  private def sendChanT(e: Expr) (implicit pos: Pos): M[Type] = e match {
    case HasType(SendChanType(t)) => Result(t)
    case _ => Problem("left operand of channel send has type %s; sending chan type required", e.t)
  }
  
  def chanRecv(chanM: M[Expr]) (implicit pos: Pos): M[Expr] =
    for (chan <- chanM; t <- recvChanT(chan))
    yield BasicExpr(chan.eval |+| ChanRecv, t)
  
  def chanSend(chanM: M[Expr], eM: M[Expr]) (implicit pos: Pos): M[CodeBuilder] = for {
    chan <- chanM
    t <- sendChanT(chan)
    e <- eM
    _ <- if (t <<= e.t) Result(())
         else Problem("type %s of right operand of channel send not assignable to element type of left operand",
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
          return Problem("%s argument has type %s not assignable to corresponding parameter type %s",
                         ordinal(index + 1), arg, param)
        Result(results.headOption getOrElse UnitType)
      }
    
    case _ => Problem("callee has type %s; function type required", callee.t)
  }
  def call(calleeM: M[Expr], argsM: M[List[Expr]]) (implicit pos: Pos): M[Expr] = for {
    callee <- calleeM
    args <- argsM
    resultT <- checkCall(callee, args)
  } yield callee.mkCall(args, resultT)
  
  
  def typeAssert(eM: M[Expr], tM: M[Type]) (implicit pos: Pos): M[Expr] = for {
    e <- eM
    t <- tM
  } yield BasicExpr(e.eval |+| TypeAssert(t), t)
}
