package jgo.compiler
package interm
package bool

import types._
import instr._
import codeseq._

import TypeConversions._

private sealed abstract class CodeParam
private sealed abstract class ProperParam { def code: CodeBuilder }
private case class  Push(b: Boolean, end: Label) extends CodeParam
private case class  Jump(target: Label)          extends CodeParam
private case object Fall                         extends CodeParam

private object ProperParam {
  def unapply(p: CodeParam): Option[CodeBuilder] = p match {
    case proper: ProperParam => Some(proper.code)
    case _ => None
  }
}

object BoolTree {
  implicit def toExpr(tree: BoolTree): Expr     = BoolExpr(tree)
  implicit def toTree(expr: BoolExpr): BoolTree = expr.tree
}

sealed abstract class BoolTree {
  def evalAsBool: CodeBuilder = {
    val g = new LabelGroup
    
    val end = new Label("end eval as bool", g)
    val t   = new Label("push true", g)
    val f   = new Label("push false", g)
    
    code(t, f) |+|
    Lbl(f) |+| BoolConst(false) |+| Goto(end) |+|
    Lbl(t) |+| BoolConst(true)  |+|
    Lbl(end)
  }
  
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
  
  protected[bool] def code(trueBr: Label, falseBr: Label): CodeBuilder
}

case object TrueTree extends BoolTree {
  protected[bool] def code(tr: Label, fl: Label): CodeBuilder = Goto(tr)
}
case object FalseTree extends BoolTree {
  protected[bool] def code(tr: Label, fl: Label): CodeBuilder = Goto(fl)
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

private[bool] sealed abstract class CompTree extends BoolTree {
  protected val e1, e2: Expr
  protected val comp: Comparison
  
  protected[bool] def cat = e1.eval |+| e2.eval
  
  protected[bool] def code(trueBr: Label, falseBr: Label): CodeBuilder =
    cat |+| Branch(comp, trueBr) |+| Goto(falseBr)
}

case class ObjEquals(e1: Expr, e2: Expr)    extends CompTree { val comp = ObjEq }
case class ObjNotEquals(e1: Expr, e2: Expr) extends CompTree { val comp = ObjNe }

case class BoolEquals(e1: Expr, e2: Expr)    extends CompTree { val comp = BoolEq }
case class BoolNotEquals(e1: Expr, e2: Expr) extends CompTree { val comp = BoolNe }


private[bool] sealed abstract class ArithCompTree(cmp: Arith => Comparison) extends CompTree {
  private val arith: Arith = e1.t.underlying.asInstanceOf[NumericType] //make better upon restruct of Expr
  protected val comp = cmp(arith)
}

case class NumEquals(e1: Expr, e2: Expr)     extends ArithCompTree(NumEq)
case class NumNotEquals(e1: Expr, e2: Expr)  extends ArithCompTree(NumNe)
case class LessThan(e1: Expr, e2: Expr)      extends ArithCompTree(NumLt)
case class GreaterThan(e1: Expr, e2: Expr)   extends ArithCompTree(NumGt)
case class LessEquals(e1: Expr, e2: Expr)    extends ArithCompTree(NumLeq)
case class GreaterEquals(e1: Expr, e2: Expr) extends ArithCompTree(NumGeq)

