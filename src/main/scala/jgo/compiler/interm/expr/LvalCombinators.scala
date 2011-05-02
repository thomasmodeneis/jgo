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

private trait LvalCombinators extends Combinators with ArithmeticTypeChecks {
  private def lval(e: Expr, desc: String) (implicit pos: Pos): M[LvalExpr] = e match {
    case l: LvalExpr => Result(l)
    case _ => Problem("lvalue expected for %s", desc)
  }
  private def mkPtr(e: Expr, desc: String) (implicit pos: Pos): M[Expr] =
    if (e.addressable) Result(e.mkPtr)
    else Problem("addressable expression expected for %s", desc)
  
  
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
  
  
  private def lvalues(es: List[Expr], desc: String) (implicit pos: Pos): M[List[LvalExpr]] = for {
    (e, i) <- es.zipWithIndex
    l <- lval(e, "%s term of %s".format(ordinal(i + 1), desc))
  } yield l //the implicit conversion Messaged.lsM2mLs lifts this List[M[LvalExpr]] to M[List[..]]
  private def zipAndCheckArity[A](as: List[A], bs: List[Expr]) (implicit pos: Pos): M[List[(A, Expr)]] = {
    var res: List[(A, Expr)] = Nil
    var (curA, curB) = (as, bs)
    var lenA, lenB = 0
    while (!curA.isEmpty || !curB.isEmpty) {
      if (curA isEmpty) {
        lenB += curB.length
        return Problem("Arity (%d) of left side of assignment unequal to arity (%d) of right side", lenA, lenB)
      }
      if (curB isEmpty) {
        lenA += curA.length
        return Problem("Arity (%d) of left side of assignment unequal to arity (%d) of right side", lenA, lenB)
      }
      val pair = (curA.head, curB.head)
      curA = curA.tail
      curB = curB.tail
      res = pair :: res
    }
    Result(res reverse)
  }
  private def checkAssignability(pairs: List[(LvalExpr, Expr)]) (implicit pos: Pos): M[Unit] = {
    for (((l, r), i) <- pairs zipWithIndex)
      if (!(l.t <<= r.t))
        return Problem(
          "%s value on right side of assignment has type %s not assignable to corresponding " +
          "target type %s", ordinal(i), r.t, l.t
        )
    Result(())
  }
  
  def assign(leftM: M[List[Expr]], rightM: M[List[Expr]]) (implicit pos: Pos): M[CodeBuilder] = for {
    (left0, right) <- together(leftM, rightM)
    left <- lvalues(left0, "left side of assignment")
    pairs <- zipAndCheckArity(left, right)
    _ <- checkAssignability(pairs)
  } yield {
    val (leftCode, rightCode) = (pairs foldLeft (CodeBuilder.empty, CodeBuilder.empty)) {
      case ((leftAcc, rightAcc), (l, r)) => (leftAcc |+| l.storePrefix(r.eval), l.storeSuffix |+| rightAcc)
    }
    leftCode |+| rightCode
  }
}
