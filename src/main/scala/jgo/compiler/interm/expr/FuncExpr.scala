package jgo.compiler
package interm
package expr

import types._
import symbol._
import instr._
import instr.TypeConversions._
import codeseq._

private case class FuncExpr(f: Function) extends Expr {
  val typeOf = f.typeOf
  override def callable = true
  
  def eval = Func2Lambda(f)
  override def call(args: List[Expr]): Either[String, Expr] =
    for (resultT <- checkCall(funcType, args).right)
    yield BasicExpr((args foldLeft CodeBuilder()) { _ |+| _.eval } |+| InvokeFunc(f), resultT)
}