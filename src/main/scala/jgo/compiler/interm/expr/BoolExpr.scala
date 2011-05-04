package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

private object BoolExpr {
  def jumpOrFall(target: Target) = target match {
    case Jump(lbl) => Goto(lbl)
    case Fall      => CodeBuilder.empty
  }
  
  sealed abstract class Target
  case class  Jump(lbl: Label) extends Target
  case object Fall             extends Target
  implicit def lbl2target(lbl: Label): Target = Jump(lbl)
}
import BoolExpr._

sealed abstract class BoolExpr extends Expr {
  val typeOf            = BoolType
  override def callable = false
  def eval = Return //placeholder
  
  private[expr] def code(trueBr: Target, falseBr: Target):  CodeBuilder
  //private[expr] def push(inverted: Boolean, end: Target): CodeBuilder
  
  def branchTo(lbl: Label): CodeBuilder = {
    val g   = new LabelGroup
    val end = new Label("end branchTo", g)
    code(lbl, end)
  }
  
  def mkIf(ifBranch: CodeBuilder): CodeBuilder = {
    val g   = new LabelGroup
    val end = new Label("end if", g)
    val t   = new Label("if branch", g)
    code(t, end) |+| Lbl(t) |+| ifBranch |+| Lbl(end)
  }
  
  def mkIfElse(ifBranch: CodeBuilder, elseBranch: CodeBuilder): CodeBuilder = {
    val g   = new LabelGroup
    val end = new Label("end if-else", g)
    val t   = new Label("if branch", g)
    val f   = new Label("else branch", g)
    code(t, f) |+| Lbl(f) |+| elseBranch |+| Goto(end) |+| Lbl(t) |+| ifBranch |+| Lbl(end)
  }
  
  def mkWhile(loopBody: CodeBuilder): CodeBuilder = {
    val g    = new LabelGroup
    val end  = new Label("end of loop", g)
    val top  = new Label("top of loop", g)
    val cond = new Label("cond of loop", g)
    Goto(cond) |+|
    Lbl(top)   |+| loopBody |+|
    Lbl(cond)  |+| code(top, end) |+|
    Lbl(end)
  }
}

private case class Not(b: BoolExpr) extends BoolExpr {
  def code(trueBr: Label, falseBr: Label): CodeBuilder =
    b.code(falseBr, trueBr)
}

private case class And(b1: BoolExpr, b2: BoolExpr) extends BoolExpr {
  def code(trueBr: Label, falseBr: Label): CodeBuilder = {
    val btwn = new Label("between and")
    b1.code(btwn, falseBr) |+| Lbl(btwn) |+| b2.code(trueBr, falseBr)
  }
}

private case class Or(b1: BoolExpr, b2: BoolExpr) extends BoolExpr {
  def code(trueBr: Label, falseBr: Label): CodeBuilder = {
    val btwn = new Label("between or")
    b1.code(trueBr, Fall) |+| Lbl(btwn) |+| b2.code(trueBr, falseBr)
  }
}

private sealed abstract class CompExpr(comp: Comparison) extends BoolExpr {
  protected val e1, e2: Expr
  
  private[expr] def code(trueBr: Label, falseBr: Label): CodeBuilder =
    e1.eval |+| e2.eval |+| Branch(comp, trueBr) |+| Goto(falseBr)
}

private case class ObjEquals   (e1: Expr, e2: Expr) extends CompExpr(ObjEq)
private case class ObjNotEquals(e1: Expr, e2: Expr) extends CompExpr(ObjNe)

private case class BoolEquals   (e1: Expr, e2: Expr) extends CompExpr(BoolEq)
private case class BoolNotEquals(e1: Expr, e2: Expr) extends CompExpr(BoolNe)

private case class NumEquals    (e1: Expr, e2: Expr, numT: Arith) extends CompExpr(NumEq(numT))
private case class NumNotEquals (e1: Expr, e2: Expr, numT: Arith) extends CompExpr(NumNe(numT))
private case class LessThan     (e1: Expr, e2: Expr, numT: Arith) extends CompExpr(NumLt(numT))
private case class GreaterThan  (e1: Expr, e2: Expr, numT: Arith) extends CompExpr(NumGt(numT))
private case class LessEquals   (e1: Expr, e2: Expr, numT: Arith) extends CompExpr(NumLeq(numT))
private case class GreaterEquals(e1: Expr, e2: Expr, numT: Arith) extends CompExpr(NumGeq(numT))

