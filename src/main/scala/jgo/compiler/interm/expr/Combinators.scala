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
  private type M[+T] = Messaged[T]
  
  def and(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Position): M[Expr]
  def or (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Position): M[Expr]
  def not(eM: M[Expr]) (implicit pos: Position): M[Expr]
  
  def compEq (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Position): M[Expr]
  def compNe (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Position): M[Expr]
  def compLt (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Position): M[Expr]
  def compLeq(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Position): M[Expr]
  def compGt (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Position): M[Expr]
  def compGeq(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Position): M[Expr]
  
  def plus (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Position): M[Expr]
  def minus(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Position): M[Expr]
  def times(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Position): M[Expr]
  def div  (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Position): M[Expr]
  def mod  (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Position): M[Expr]
  def pos(eM: M[Expr]) (implicit pos: Position): M[Expr]
  def neg(eM: M[Expr]) (implicit pos: Position): M[Expr]
  
  def bitAnd   (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Position): M[Expr]
  def bitAndNot(e1M: M[Expr], e2M: M[Expr]) (implicit pos: Position): M[Expr]
  def bitOr    (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Position): M[Expr]
  def bitXor   (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Position): M[Expr]
  def shiftL   (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Position): M[Expr]
  def shiftR   (e1M: M[Expr], e2M: M[Expr]) (implicit pos: Position): M[Expr]
  def compl(eM: M[Expr]) (implicit pos: Position): M[Expr]
  
  def addrOf(eM: M[Expr]) (implicit pos: Position): M[Expr]
  def deref (eM: M[Expr]) (implicit pos: Position): M[Expr]
  
  def chanRecv(chanM: M[Expr]) (implicit pos: Position): M[Expr]
  def chanSend(chanM: M[Expr], eM: M[Expr]) (implicit pos: Position): M[CodeBuilder]
  
  def select    (objM:  M[Expr], selector: String)        (implicit pos: Position): M[Expr]
  def call      (funcM: M[Expr], argsM:    M[List[Expr]]) (implicit pos: Position): M[Expr]
  def typeAssert(eM:    M[Expr], tM:       M[Type])       (implicit pos: Position): M[Expr]
  
  def subscript(arrM: M[Expr], indxM: M[Expr]) (implicit pos: Position): M[Expr]
  def slice(arrM: M[Expr], lowM: M[Option[Expr]], highM: M[Option[Expr]]) (implicit pos: Position): M[Expr]
  
  def incr(eM: M[Expr]) (implicit pos: Position): M[CodeBuilder]
  def decr(eM: M[Expr]) (implicit pos: Position): M[CodeBuilder]
  
  def assign(leftM: M[Expr], rightM: M[Expr]) (implicit pos: Position): M[CodeBuilder]
  
  def eval(eM: M[Expr]): M[CodeBuilder] = for (e <- eM) yield e.eval
}

object Combinators //extends Combinators
