package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

private object BoolExpr {
  sealed abstract class Target {
    def replaceFall(end: Label): Target = this match {
      case j: Jump => j
      case Fall    => Jump(end)
    }
    def isJump = this.isInstanceOf[Jump]
  }
  
  case class  Jump(lbl: Label) extends Target
  case object Fall             extends Target
  
  implicit def lbl2target(lbl: Label): Target = Jump(lbl)
}
import BoolExpr._

sealed abstract class BoolExpr extends Expr {
  val typeOf            = BoolType
  override def callable = false
  
  private[expr] def branch(trueBr: Target, falseBr: Target): CodeBuilder
  
  def eval = {
    val g   = new LabelGroup
    val tr  = new Label("push true", g)
    val end = new Label("end of push bool", g)
    
    branch(tr, Fall) |+| PushBool(false) |+| Goto(end) |+| Lbl(tr) |+| PushBool(true) |+| Lbl(end)
  }
  
  def branchTo(lbl: Label): CodeBuilder = {
    val g   = new LabelGroup
    val end = new Label("end branchTo", g)
    branch(lbl, Fall)
  }
  
  def mkIf(ifBranch: CodeBuilder): CodeBuilder = {
    val g   = new LabelGroup
    val end = new Label("end if", g)
    val t   = new Label("if branch", g)
    branch(Fall, end) |+| Lbl(t) |+| ifBranch |+| Lbl(end)
  }
  
  def mkIfElse(ifBranch: CodeBuilder, elseBranch: CodeBuilder): CodeBuilder = {
    val g   = new LabelGroup
    val end = new Label("end if-else", g)
    val t   = new Label("if branch", g)
    val f   = new Label("else branch", g)
    branch(t, Fall) |+| Lbl(f) |+| elseBranch |+| Goto(end) |+| Lbl(t) |+| ifBranch |+| Lbl(end)
  }
  
  def mkWhile(loopBody: CodeBuilder): CodeBuilder = {
    val g    = new LabelGroup
    val end  = new Label("end of loop", g)
    val top  = new Label("top of loop", g)
    val cond = new Label("cond of loop", g)
    Goto(cond) |+|
    Lbl(top)   |+| loopBody |+|
    Lbl(cond)  |+| branch(top, Fall) |+|
    Lbl(end)
  }
}

private class BoolValueExpr(evalCode: => CodeBuilder) extends BoolExpr {
  override def eval = evalCode
  
  def branch(t: Target, f: Target) = (t, f) match {
    case (Jump(tLbl), Jump(fLbl)) => evalCode |+| Branch(IsTrue, tLbl) |+| Goto(fLbl)
    case (Jump(tLbl), Fall)       => evalCode |+| Branch(IsTrue, tLbl)
    case (Fall,       Jump(fLbl)) => evalCode |+| Branch(IfNot(IsTrue), fLbl)
    
    case (Fall, Fall) => throw new AssertionError("impl error: no reason why both branches should be Fall")
  }
}

private class Not(b: BoolExpr) extends BoolExpr {
  def branch(trueBr: Target, falseBr: Target): CodeBuilder =
    b.branch(falseBr, trueBr)
}

private class And(b1: BoolExpr, b2: BoolExpr) extends BoolExpr {
  def branch(trueBr: Target, falseBr: Target): CodeBuilder = {
    val g    = new LabelGroup
    val btwn = new Label("between and", g)
    val end  = new Label("end and", g)
    
    b1.branch(Fall, falseBr.replaceFall(end)) |+| Lbl(btwn) |+| b2.branch(trueBr, falseBr)
  }
}

private class Or(b1: BoolExpr, b2: BoolExpr) extends BoolExpr {
  def branch(trueBr: Target, falseBr: Target): CodeBuilder = {
    val g    = new LabelGroup
    val btwn = new Label("between or", g)
    val end  = new Label("end or", g)
    
    b1.branch(trueBr.replaceFall(end), Fall) |+| Lbl(btwn) |+| b2.branch(trueBr, falseBr)
  }
}

private sealed abstract class CompExpr(comp: Comparison) extends BoolExpr {
  protected val e1, e2: Expr
  
  private val stackingCode = e1.eval |+| e2.eval
  
  private[expr] def branch(trueBr: Target, falseBr: Target): CodeBuilder = (trueBr, falseBr) match {
    case (Jump(tLbl), Jump(fLbl)) => e1.eval |+| e2.eval |+| Branch(comp, tLbl) |+| Goto(fLbl)
    case (Jump(tLbl), Fall)       => e1.eval |+| e2.eval |+| Branch(comp, tLbl)
    case (Fall,       Jump(fLbl)) => e1.eval |+| e2.eval |+| Branch(IfNot(comp), fLbl)
    
    case (Fall, Fall) => throw new AssertionError("impl error: no reason why both branches should be Fall")
  }
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

