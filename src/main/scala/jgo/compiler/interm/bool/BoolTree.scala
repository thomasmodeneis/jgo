package jgo.compiler
package interm
package bool

import types._
import instr._
import codeseq._
import CodeBuilder.fromInstr

import TypeConversions._

object BoolTree {
  implicit def toExpr(tree: BoolTree): Expr     = BoolExpr(tree)
  implicit def toTree(expr: BoolExpr): BoolTree = expr.tree
}

sealed abstract class BoolTree {
  def evalAsBool: CodeBuilder = {
    val end = new Label("end pushBool")
    val t   = new Label("push true")
    val f   = new Label("push false")
    
    code(t, f) |+|
    Lbl(f) |+| BoolConst(false) |+| Goto(end) |+|
    Lbl(t) |+| BoolConst(true)  |+|
    Lbl(end)
  }
  
  def branchTo(lbl: Label) = {
    val end = new Label("end branchTo")
    code(lbl, end)
  }
  
  def mkIf(ifBranch: CodeBuilder) = {
    val end = new Label("end if")
    val t   = new Label("if: true branch")
    code(t, end) |+| Lbl(t) |+| ifBranch |+| Lbl(end)
  }
  
  def mkIfElse(ifBranch: CodeBuilder, elseBranch: CodeBuilder) = {
    val end = new Label("end if-else")
    val t   = new Label("if-else: true branch")
    val f   = new Label("if-else: false branch")
    code(t, f) |+| Lbl(f) |+| elseBranch |+| Goto(end) |+| Lbl(t) |+| ifBranch |+| Lbl(end)
  }
  
  def mkWhile(loopBody: CodeBuilder) = {
    val end  = new Label("end of loop")
    val top  = new Label("top of loop")
    val cond = new Label("condition of loop")
    Goto(cond) |+|
    Lbl(top)   |+| loopBody |+|
    Lbl(cond)  |+| code(top, end) |+|
    Lbl(end)
  }
  
  protected[bool] def code(trueBr: Label, falseBr: Label): CodeBuilder
}

case class Not(b: BoolTree) extends BoolTree {
  protected[bool] def code(trueBr: Label, falseBr: Label): CodeBuilder =
    b.code(falseBr, trueBr)
}

case class And(b1: BoolTree, b2: BoolTree) extends BoolTree {
  protected[bool] def code(trueBr: Label, falseBr: Label): CodeBuilder = {
    val btwn = new Label("between and")
    b1.code(btwn, falseBr) |+| Lbl(btwn) |+| b2.code(trueBr, falseBr)
  }
}

case class Or(b1: BoolTree, b2: BoolTree) extends BoolTree {
  protected[bool] def code(trueBr: Label, falseBr: Label): CodeBuilder = {
    val btwn = new Label("between or")
    b1.code(trueBr, btwn) |+| Lbl(btwn) |+| b2.code(trueBr, falseBr)
  }
}

private[bool] sealed abstract class Comparison(branch: Label => Instr) extends BoolTree with Typed {
  val e1, e2: Expr
  require(e1.t == e2.t)
  val typeOf = e1.t
  protected[bool] def comp = e1.eval |+| e2.eval
  
  protected[bool] def code(trueBr: Label, falseBr: Label): CodeBuilder =
    comp |+| branch(trueBr) |+| Goto(falseBr)
}

case class ObjEquals(e1: Expr, e2: Expr)    extends Comparison(BranchObjEq)
case class ObjNotEquals(e1: Expr, e2: Expr) extends Comparison(BranchObjNe)

case class BoolEquals(e1: Expr, e2: Expr)    extends Comparison(BranchBoolEq)
case class BoolNotEquals(e1: Expr, e2: Expr) extends Comparison(BranchBoolNe)


private[bool] sealed abstract class NumericComparison(branch: Label => Instr) extends Comparison(branch) {
  require(isOfType[NumericType])
  val numerT: NumericType     = t.underlying.asInstanceOf[NumericType]
  override protected[bool] def comp = e1.eval |+| e2.eval |+| Compare(numerT)
}

case class NumEquals(e1: Expr, e2: Expr)     extends NumericComparison(BranchEq)
case class NumNotEquals(e1: Expr, e2: Expr)  extends NumericComparison(BranchNe)

case class LessThan(e1: Expr, e2: Expr)      extends NumericComparison(BranchLt)
case class GreaterThan(e1: Expr, e2: Expr)   extends NumericComparison(BranchGt)
case class LessEquals(e1: Expr, e2: Expr)    extends NumericComparison(BranchLeq)
case class GreaterEquals(e1: Expr, e2: Expr) extends NumericComparison(BranchGeq)

