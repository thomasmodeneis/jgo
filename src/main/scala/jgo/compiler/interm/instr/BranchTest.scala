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
  
  protected def strPrefix: String
  protected def strBody:   String
  
  final override val toString = strPrefix + " " + strBody
}

case class IfNot(cond: SimpleTest) extends BranchTest {
  protected def strPrefix = "ifNot"
  protected def strBody   = cond.strBody
  
  val arity = cond.arity
  val t     = cond.t
}

sealed abstract class SimpleTest private[instr] (val arity: Int, body: String) extends BranchTest {
  protected[instr] def strPrefix = "if"
  protected[instr] def strBody   = body + " " + t
}

private[instr] sealed abstract class EasyTest(val t: StackType, a: Int, b: String) extends SimpleTest(a, b)

case object IsTrue  extends EasyTest(Bool, 1, "true")
case object IsFalse extends EasyTest(Bool, 1, "true")

case object BoolEq extends EasyTest(Bool, 2, "==")
case object BoolNe extends EasyTest(Bool, 2, "!=")

case object ObjEq extends EasyTest(Obj, 2, "==")
case object ObjNe extends EasyTest(Obj, 2, "!=")


private[instr] sealed abstract class ArithTest(body: String) extends SimpleTest(2, body)

case class NumEq(t: Arith)  extends ArithTest("==")
case class NumNe(t: Arith)  extends ArithTest("!=")
case class NumLt(t: Arith)  extends ArithTest(">")
case class NumLeq(t: Arith) extends ArithTest("<=")
case class NumGt(t: Arith)  extends ArithTest(">")
case class NumGeq(t: Arith) extends ArithTest(">=")


