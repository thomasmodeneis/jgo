package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import symbol._
import codeseq._

trait Combinators {
  def and(e1: Expr, e2: Expr)(pos: Pos): M[Expr]
  def or (e1: Expr, e2: Expr)(pos: Pos): M[Expr]
  def not(e: Expr)(pos: Pos): M[Expr]
  
  def compEq (e1: Expr, e2: Expr)(pos: Pos): M[Expr]
  def compNe (e1: Expr, e2: Expr)(pos: Pos): M[Expr]
  def compLt (e1: Expr, e2: Expr)(pos: Pos): M[Expr]
  def compLeq(e1: Expr, e2: Expr)(pos: Pos): M[Expr]
  def compGt (e1: Expr, e2: Expr)(pos: Pos): M[Expr]
  def compGeq(e1: Expr, e2: Expr)(pos: Pos): M[Expr]
  
  def plus (e1: Expr, e2: Expr)(pos: Pos): M[Expr]
  def minus(e1: Expr, e2: Expr)(pos: Pos): M[Expr]
  def times(e1: Expr, e2: Expr)(pos: Pos): M[Expr]
  def div  (e1: Expr, e2: Expr)(pos: Pos): M[Expr]
  def mod  (e1: Expr, e2: Expr)(pos: Pos): M[Expr]
  def positive(e: Expr)(pos: Pos): M[Expr]
  def negative(e: Expr)(pos: Pos): M[Expr]
  
  def bitAnd   (e1: Expr, e2: Expr)(pos: Pos): M[Expr]
  def bitAndNot(e1: Expr, e2: Expr)(pos: Pos): M[Expr]
  def bitOr    (e1: Expr, e2: Expr)(pos: Pos): M[Expr]
  def bitXor   (e1: Expr, e2: Expr)(pos: Pos): M[Expr]
  def shiftL   (e1: Expr, e2: Expr)(pos: Pos): M[Expr]
  def shiftR   (e1: Expr, e2: Expr)(pos: Pos): M[Expr]
  def bitCompl(e: Expr)(pos: Pos): M[Expr]
  
  def addrOf(e: Expr)(pos: Pos): M[Expr]
  def deref (e: Expr)(pos: Pos): M[Expr]
  
  def chanRecv(chan: Expr)(pos: Pos): M[Expr]
  def chanSend(chan: Expr, e: Expr)(pos: Pos): M[CodeBuilder]
  
  //def select(obj:  Expr, selector: String)(implicit pos: Pos): M[Expr]
  def invoke(callee: Expr, args: List[Expr])(pos: Pos): M[Expr]
  def typeAssert(e: Expr, t: Type)(pos: Pos): M[Expr]
  
  def index(arr: Expr, indx: Expr)(pos: Pos): M[Expr]
  def slice(arr: Expr, low: Option[Expr], high: Option[Expr])(pos: Pos): M[Expr]
  
  def incr(e: Expr)(pos: Pos): M[CodeBuilder]
  def decr(e: Expr)(pos: Pos): M[CodeBuilder]
  
  def assign(left: List[Expr], right: List[Expr])(pos: Pos): M[CodeBuilder]
  def assign(left: Expr, right: Expr)(pos: Pos): M[CodeBuilder]
  
  def assignableTo(e: Expr, targetType: Type)(pos: Pos): M[Expr]
  
  protected def convertForAssign(e: Expr, t: Type, desc: String)(pos: Pos): M[Expr]
  def convert(e: Expr, t: Type)(pos: Pos): M[Expr]
  
  def constant   (e: Expr)(pos: Pos): M[ConstExpr]
  def conditional(e: Expr)(pos: Pos): M[ConditionalExpr]
  
  def fromVariable(v: Variable): Expr = VarLval(v)
  def fromFunction(f: Function): Expr = FunctionExpr(f)
  
  def eval(e: Expr) =
    e.eval
}

object Combinators extends ConversionCombinators
                      with BasicCombinators
                      with ConditionalCombinators
                      with LvalCombinators
                      with ConstCombinators
