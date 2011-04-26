package jgo.compiler
package interm
package instr

/**
 * A test that may be used as the condition of a branch instruction.
 */
sealed abstract class BranchTest {
  /**
   * The number of items removed from the top of the operand stack
   * in the course of evaluating this test.
   */
  val arity: Int
  
  /**
   * The stack-type of the items this test requires be at the top
   * of the operand stack, which items are popped in evaluating
   * this test.
   */
  val t: StackType
  
  protected[instr] def prefix: String
  protected[instr] def name:   String
  
  final override val toString = prefix + " " + name + " " + t
}

case class IfNot(cond: SimpleTest) extends BranchTest {
  protected[instr] def prefix = "ifNot"
  protected[instr] def name   = cond.name
  
  val arity = cond.arity
  val t     = cond.t
}

sealed abstract class SimpleTest(val arity: Int) extends BranchTest {
  protected[instr] def prefix = "if"
}
sealed abstract class Comparison extends SimpleTest(2)
sealed abstract class Check      extends SimpleTest(1)


case object IsTrue  extends Check { val t = Bool; def name = "is true"  }
case object IsFalse extends Check { val t = Bool; def name = "is false" }

case object BoolEq extends Comparison { val t = Bool; def name = "==" }
case object BoolNe extends Comparison { val t = Bool; def name = "!=" }

case object ObjEq extends Comparison { val t = Obj; def name = "==" }
case object ObjNe extends Comparison { val t = Obj; def name = "!=" }


case class NumEq(t: Arith)  extends Comparison { def name = "==" }
case class NumNe(t: Arith)  extends Comparison { def name = "!=" }
case class NumLt(t: Arith)  extends Comparison { def name = "<"  }
case class NumLeq(t: Arith) extends Comparison { def name = "<=" }
case class NumGt(t: Arith)  extends Comparison { def name = ">"  }
case class NumGeq(t: Arith) extends Comparison { def name = ">=" }


