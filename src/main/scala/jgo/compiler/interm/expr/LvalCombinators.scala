package jgo.compiler
package interm
package expr

import message._
import message.Messaged._

import types._
import instr._
import instr.TypeConversions._
import codeseq._

private trait LvalCombinators extends Combinators with ArithmeticTypeChecks {
  private def lval(e: Expr, desc: String) (implicit pos: Pos): M[LvalExpr] = e match {
    case l: LvalExpr => Result(l)
    case _ => Problem("%s must be an lvalue", desc)
  }
  private def mkPtr(e: Expr, desc: String) (implicit pos: Pos): M[Expr] =
    if (e.addressable) Result(e.mkPtr)
    else Problem("%s must be an addressable expression", desc)
  
  def addrOf(eM: M[Expr]) (implicit pos: Pos): M[Expr] = for {
    e <- eM
    p <- mkPtr(e, "operand")
  } yield p
  
  def deref(eM: M[Expr]) (implicit pos: Pos): M[PtrDerefLval] = for {
    e   <- eM
    res <- e match {
      case HasType(PointerType(elemT)) => Result(PtrDerefLval(e, elemT))
      case _ =>
        Problem("operand of pointer indirection/dereference has type %s; pointer type required", e.t) 
    }
  } yield res
  
  def select(objM: M[Expr], selector: String) (implicit pos: Pos): M[Expr]
  
  def subscript(arrM: M[Expr], indxM: M[Expr]) (implicit pos: Pos): M[Expr]
  def slice(arrM: M[Expr], lowM: M[Option[Expr]], highM: M[Option[Expr]]) (implicit pos: Pos): M[Expr]
  
  def incr(eM: M[Expr]) (implicit pos: Pos): M[CodeBuilder] = for {
    e <- eM
    l <- lval(e, "operand of ++")
    intT <- integral(l, "operand of ++")
  } yield l match {
    case VarLval(vr) => Incr(vr, 1, intT)
    case _ => l.store(l.load |+| IntConst(1, intT) |+| Add(intT))
  }
  
  def decr(eM: M[Expr]) (implicit pos: Pos): M[CodeBuilder] = for {
    e <- eM
    l <- lval(e, "operand of --")
    intT <- integral(l, "operand of --")
  } yield l match {
    case VarLval(vr) => Decr(vr, 1, intT)
    case _ => l.store(l.load |+| IntConst(1, intT) |+| Sub(intT))
  }
  
  def assign(leftM: M[List[Expr]], rightM: M[List[Expr]]) (implicit pos: Pos): M[CodeBuilder] = {
    for {
    
    }
  }
}
