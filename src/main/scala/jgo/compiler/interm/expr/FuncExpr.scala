package jgo.compiler
package interm
package expr

import types._
import symbol._
import instr._
import instr.TypeConversions._
import codeseq._

private case class FunctionExpr(f: Function) extends Expr {
  val typeOf: FuncType = f.typeOf
  override def callable = true
  
  def eval = Func2Lambda(f)
  
  override def mkCall(args: List[Expr], resultT: Type): Expr =
    BasicExpr((args foldLeft CodeBuilder()) { _ |+| _.eval } |+| InvokeFunction(f), resultT)
}
