package jgo.compiler
//package message

import message._
import message.Messaged._
import interm._
import codeseq._
import instr._
import types._

import scala.util.parsing.input.{Position, NoPosition}


object MessagedTest {
  implicit val pos: Position = NoPosition
  private type M[+T] = Messaged[T]
  
  def test1() {
    val e1M: M[Expr] = Problem("operands of * have differing types int and int32")
    val e2M: M[Expr] = SimpleExpr(IntConst(2, I32), Int32)
    
    
  }
  
  def plus(e1M: M[Expr], e2M: M[Expr]): M[Expr] = for {
    (e1, e2) <- together(e1M, e2M)
    res <-
      if (e1.t != e2.t)
        Problem("operands of + have differing types %s and %s", e1.t, e2.t)
      else if (!e1.isOfType[NumericType] && !e1.isOfType[StringType])
        Problem("left operand of + has type %s, which is not a numeric or string type", e1.t)
      else if (!e2.isOfType[NumericType] && !e2.isOfType[StringType])
        Problem("right operand of + has type %s, which is not a numeric or string type", e2.t)
      else e1 match {
        case HasType(t: NumericType) => SimpleExpr(e1.eval |+| e2.eval |+| Add(t), e1.t)
        case HasType(StringType)     => SimpleExpr(e1.eval |+| e2.eval |+| StrAdd, e1.t)
      }
  }
  yield res
}
