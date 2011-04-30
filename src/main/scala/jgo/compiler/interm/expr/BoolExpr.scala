package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

/*
private case class BoolExpr(tree: BoolTree) extends Expr {
  val typeOf            = BoolType
  override def callable = false
  def eval = tree.evalAsBool
  
  def branchTo(lbl: Label)                           = tree.branchTo(lbl)
  def mkIf(ifB: CodeBuilder)                         = tree.mkIf(ifB)
  def mkIfElse(ifB: CodeBuilder, elseB: CodeBuilder) = tree.mkIfElse(ifB, elseB)
  def mkWhile(loopBody: CodeBuilder)                 = tree.mkWhile(loopBody)
}
*/
