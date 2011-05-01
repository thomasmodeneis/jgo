package jgo.compiler
package interm
package expr

import message._
import message.Messaged._

import types._
import instr._
import instr.TypeConversions._
import codeseq._

private trait LvalCombinators extends Combinators {
  private def lval(e: Expr, desc: String) (implicit pos: Pos): M[LvalExpr] = e match {
    case l: LvalExpr => Result(l)
    case _ => Problem("%s is not an lvalue", desc)
  }
  private def mkPtr(e: Expr, desc: String) (implicit pos: Pos): M[Expr] =
    if (e.addressable) Result(e.mkPtr)
    else Problem("%s is not an addressable expression", desc)
  private def ptrElemType(ptr: Expr, desc: String)
  
  def addrOf(eM: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    e <- eM
    p <- mkPtr(e, "operand of address-of"
  }
  def deref(eM: M[Expr])(implicit pos: Pos): M[Expr]
  
  def select(objM: M[Expr], selector: String) (implicit pos: Pos): M[Expr]
  
  def subscript(arrM: M[Expr], indxM: M[Expr]) (implicit pos: Pos): M[Expr]
  def slice(arrM: M[Expr], lowM: M[Option[Expr]], highM: M[Option[Expr]]) (implicit pos: Pos): M[Expr]
  
  def incr(eM: M[Expr]) (implicit pos: Pos): M[CodeBuilder]
  def decr(eM: M[Expr]) (implicit pos: Pos): M[CodeBuilder]
  
  def assign(leftM: M[Expr], rightM: M[Expr]) (implicit pos: Pos): M[CodeBuilder]
}
