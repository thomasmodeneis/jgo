package jgo.compiler
//package message

import message._
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
    
    for {
      (e1, e2) <- together(e1M, e2M)
      res <- {
        
      }
    }
    yield res
  }
}
