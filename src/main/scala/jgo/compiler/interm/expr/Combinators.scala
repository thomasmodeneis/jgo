package jgo.compiler
package interm
package expr

import message._

import types._
import instr._
import instr.TypeConversions._
import codeseq._

import scala.util.parsing.input.Position

trait Combinators {
  def and(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  def or (e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  def not(e: Expr) (implicit pos: Pos): M[Expr]
  
  def compEq (e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  def compNe (e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  def compLt (e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  def compLeq(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  def compGt (e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  def compGeq(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  
  def plus (e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  def minus(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  def times(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  def div  (e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  def mod  (e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  def pos(e: Expr) (implicit pos: Pos): M[Expr]
  def neg(e: Expr) (implicit pos: Pos): M[Expr]
  
  def bitAnd   (e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  def bitAndNot(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  def bitOr    (e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  def bitXor   (e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  def shiftL   (e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  def shiftR   (e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  def bitCompl(e: Expr) (implicit pos: Pos): M[Expr]
  
  def addrOf(e: Expr) (implicit pos: Pos): M[Expr]
  def deref (e: Expr) (implicit pos: Pos): M[Expr]
  
  def chanRecv(chan: Expr) (implicit pos: Pos): M[Expr]
  def chanSend(chan: Expr, e: Expr) (implicit pos: Pos): M[CodeBuilder]
  
  //def select(obj:  Expr, selector: String) (implicit pos: Pos): M[Expr]
  def invoke(callee: Expr, args: List[Expr]) (implicit pos: Pos): M[Expr]
  def typeAssert(e: Expr, t: Type) (implicit pos: Pos): M[Expr]
  
  def index(arr: Expr, indx: Expr) (implicit pos: Pos): M[Expr]
  def slice(arr: Expr, low: Option[Expr], high: Option[Expr]) (implicit pos: Pos): M[Expr]
  
  def incr(e: Expr) (implicit pos: Pos): M[CodeBuilder]
  def decr(e: Expr) (implicit pos: Pos): M[CodeBuilder]
  
  def assign(left: List[Expr], right: List[Expr]) (implicit pos: Pos): M[CodeBuilder]
  
  def eval(e: Expr): M[CodeBuilder] =
    e.eval
}

object Combinators //extends Combinators
