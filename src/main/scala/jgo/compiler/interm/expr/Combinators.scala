package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import symbol._
import codeseq._

trait Combinators {
  def const(e: Expr) (implicit pos: Pos): M[Expr]
  
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
  def positive(e: Expr) (implicit pos: Pos): M[Expr]
  def negative(e: Expr) (implicit pos: Pos): M[Expr]
  
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
  
  def assignableTo(e: Expr, targetType: Type) (implicit pos: Pos): M[Expr] =
    if (targetType <<= e.typeOf) Result(e)
    else Problem("expression not assignable to target type %s", targetType)
  
  def conditional(e: Expr) (implicit pos: Pos): M[ConditionalExpr]
  
  def fromVariable(v: Variable): Expr = VarLval(v)
  def fromFunction(f: Function): Expr = FunctionExpr(f)
  
  def eval(e: Expr) =
    e.eval
}

object Combinators {
  private val c = new BasicCombinators with ConditionalCombinators with LvalCombinators with ConstCombinators
  
  def const(e: Expr)(pos: Pos): M[Expr] = c.const(e)(pos)
  
  def and(e1: Expr, e2: Expr)(pos: Pos): M[Expr] = c.and(e1, e2)(pos)
  def or (e1: Expr, e2: Expr)(pos: Pos): M[Expr] = c.or(e1, e2)(pos)
  def not(e: Expr)(pos: Pos): M[Expr] = c.not(e)(pos)
  
  def compEq (e1: Expr, e2: Expr)(pos: Pos): M[Expr] = c.compEq(e1, e2)(pos)
  def compNe (e1: Expr, e2: Expr)(pos: Pos): M[Expr] = c.compNe(e1, e2)(pos)
  def compLt (e1: Expr, e2: Expr)(pos: Pos): M[Expr] = c.compLt(e1, e2)(pos)
  def compLeq(e1: Expr, e2: Expr)(pos: Pos): M[Expr] = c.compLeq(e1, e2)(pos)
  def compGt (e1: Expr, e2: Expr)(pos: Pos): M[Expr] = c.compGt(e1, e2)(pos)
  def compGeq(e1: Expr, e2: Expr)(pos: Pos): M[Expr] = c.compGeq(e1, e2)(pos)
  
  def plus (e1: Expr, e2: Expr)(pos: Pos): M[Expr] = c.plus(e1, e2)(pos)
  def minus(e1: Expr, e2: Expr)(pos: Pos): M[Expr] = c.minus(e1, e2)(pos)
  def times(e1: Expr, e2: Expr)(pos: Pos): M[Expr] = c.times(e1, e2)(pos)
  def div  (e1: Expr, e2: Expr)(pos: Pos): M[Expr] = c.div(e1, e2)(pos)
  def mod  (e1: Expr, e2: Expr)(pos: Pos): M[Expr] = c.mod(e1, e2)(pos)
  def positive(e: Expr)(pos: Pos): M[Expr] = c.positive(e)(pos)
  def negative(e: Expr)(pos: Pos): M[Expr] = c.negative(e)(pos)
  
  def bitAnd   (e1: Expr, e2: Expr)(pos: Pos): M[Expr] = c.bitAnd(e1, e2)(pos)
  def bitAndNot(e1: Expr, e2: Expr)(pos: Pos): M[Expr] = c.bitAndNot(e1, e2)(pos)
  def bitOr    (e1: Expr, e2: Expr)(pos: Pos): M[Expr] = c.bitOr(e1, e2)(pos)
  def bitXor   (e1: Expr, e2: Expr)(pos: Pos): M[Expr] = c.bitXor(e1, e2)(pos)
  def shiftL   (e1: Expr, e2: Expr)(pos: Pos): M[Expr] = c.shiftL(e1, e2)(pos)
  def shiftR   (e1: Expr, e2: Expr)(pos: Pos): M[Expr] = c.shiftR(e1, e2)(pos)
  def bitCompl(e: Expr)(pos: Pos): M[Expr] = c.bitCompl(e)(pos)
  
  def addrOf(e: Expr)(pos: Pos): M[Expr] = c.addrOf(e)(pos)
  def deref (e: Expr)(pos: Pos): M[Expr] = c.deref(e)(pos)
  
  def chanRecv(chan: Expr)(pos: Pos): M[Expr] = c.chanRecv(chan)(pos)
  def chanSend(chan: Expr, e: Expr)(pos: Pos): M[CodeBuilder] = c.chanSend(chan, e)(pos)
  
  //def select(obj:  Expr, selector: String) (pos: Pos): M[Expr] = c.select(obj, selector)(pos)
  def invoke(callee: Expr, args: List[Expr])(pos: Pos): M[Expr] = c.invoke(callee, args)(pos)
  def typeAssert(e: Expr, t: Type)(pos: Pos): M[Expr] = c.typeAssert(e, t)(pos)
  
  def index(arr: Expr, indx: Expr)(pos: Pos): M[Expr] = c.index(arr, indx)(pos)
  def slice(arr: Expr, low: Option[Expr], high: Option[Expr])(pos: Pos): M[Expr] = c.slice(arr, low, high)(pos)
  
  def incr(e: Expr)(pos: Pos): M[CodeBuilder] = c.incr(e)(pos)
  def decr(e: Expr)(pos: Pos): M[CodeBuilder] = c.decr(e)(pos)
  
  def assign(left: List[Expr], right: List[Expr])(pos: Pos): M[CodeBuilder] = c.assign(left, right)(pos)
  
  def conditional(e: Expr)(pos: Pos): M[ConditionalExpr] = c.conditional(e)(pos)
  
  def fromVariable(v: Variable): Expr = c.fromVariable(v)
  def fromFunction(f: Function): Expr = c.fromFunction(f)
  
  def eval(e: Expr) = c.eval(e)
}
