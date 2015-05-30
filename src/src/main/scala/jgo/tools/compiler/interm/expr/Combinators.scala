package jgo.tools.compiler
package interm
package expr

import combin._

import types._
import instr._
import instr.TypeConversions._
import symbol._
import codeseq._

trait Combinators {
  def and(e1: Expr, e2: Expr)(pos: Pos): Err[Expr]
  def or (e1: Expr, e2: Expr)(pos: Pos): Err[Expr]
  def not(e: Expr)(pos: Pos): Err[Expr]
  
  def compEq (e1: Expr, e2: Expr)(pos: Pos): Err[Expr]
  def compNe (e1: Expr, e2: Expr)(pos: Pos): Err[Expr]
  def compLt (e1: Expr, e2: Expr)(pos: Pos): Err[Expr]
  def compLeq(e1: Expr, e2: Expr)(pos: Pos): Err[Expr]
  def compGt (e1: Expr, e2: Expr)(pos: Pos): Err[Expr]
  def compGeq(e1: Expr, e2: Expr)(pos: Pos): Err[Expr]
  
  def plus (e1: Expr, e2: Expr)(pos: Pos): Err[Expr]
  def minus(e1: Expr, e2: Expr)(pos: Pos): Err[Expr]
  def times(e1: Expr, e2: Expr)(pos: Pos): Err[Expr]
  def div  (e1: Expr, e2: Expr)(pos: Pos): Err[Expr]
  def mod  (e1: Expr, e2: Expr)(pos: Pos): Err[Expr]
  def positive(e: Expr)(pos: Pos): Err[Expr]
  def negative(e: Expr)(pos: Pos): Err[Expr]
  
  def bitAnd   (e1: Expr, e2: Expr)(pos: Pos): Err[Expr]
  def bitAndNot(e1: Expr, e2: Expr)(pos: Pos): Err[Expr]
  def bitOr    (e1: Expr, e2: Expr)(pos: Pos): Err[Expr]
  def bitXor   (e1: Expr, e2: Expr)(pos: Pos): Err[Expr]
  def shiftL   (e1: Expr, e2: Expr)(pos: Pos): Err[Expr]
  def shiftR   (e1: Expr, e2: Expr)(pos: Pos): Err[Expr]
  def bitCompl(e: Expr)(pos: Pos): Err[Expr]
  
  def addrOf(e: Expr)(pos: Pos): Err[Expr]
  def deref (e: Expr)(pos: Pos): Err[Expr]
  
  def chanRecv(chan: Expr)(pos: Pos): Err[Expr]
  def chanSend(chan: Expr, e: Expr)(pos: Pos): Err[CodeBuilder]
  
  def select(obj: Expr, selector: String)(pos: Pos): Err[Expr]
  def invoke(callee: Expr, args: List[Expr])(pos: Pos): Err[Expr]
  
  def typeAssert(e: Expr, t: Type)(pos: Pos): Err[Expr]
  
  def index(arr: Expr, indx: Expr)(pos: Pos): Err[Expr]
  def slice(arr: Expr, low: Option[Expr], high: Option[Expr])(pos: Pos): Err[Expr]
  
  def incr(e: Expr)(pos: Pos): Err[CodeBuilder]
  def decr(e: Expr)(pos: Pos): Err[CodeBuilder]
  
  def assign(left: List[Expr], right: List[Expr])(pos: Pos): Err[CodeBuilder]
  def assign(left: Expr, right: Expr)(pos: Pos): Err[CodeBuilder]
  
  def assignableTo(e: Expr, targetType: Type)(pos: Pos): Err[Expr]
  
  protected def convertForAssign(e: Expr, t: Type, desc: String)(pos: Pos): Err[Expr]
  def convert(e: Expr, t: Type)(pos: Pos): Err[Expr]
  
  def constant   (e: Expr)(pos: Pos): Err[ConstExpr]
  def conditional(e: Expr)(pos: Pos): Err[ConditionalExpr]
  
  def fromVariable(v: Variable): Expr = VarLval(v)
  def fromFunction(f: Function): Expr = FunctionExpr(f)
  
  def eval(e: Expr): CodeBuilder
}

object Combinators
  extends ConversionCombinators
  with BasicCombinators
  with ConditionalCombinators
  with LvalCombinators
  with ConstCombinators
