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
  
  /**
   * The name of this test, which is used in code listings.
   */
  protected[instr] def name: String
  
  final override def toString = name + " " + t
}

/** A test of arity two. */
sealed abstract class Comparison extends BranchTest { val arity = 2 }
/** A test of arity one. */
sealed abstract class Check extends BranchTest { val arity = 1 }

case object IsTrue  extends Check { val t = Bool; def name = "is true"  }

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


