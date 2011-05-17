package jgo.compiler
package interm.instr

/**
 * A value indicating which bounds have been specified
 * in a certain slicing and which have been left blank.
 * An instance of this class designates how many operands
 * are to be popped off of the stack and how they are to
 * be interpreted in the course of execution of a given
 * slicing instruction.
 */
sealed abstract class SliceBounds(val numberOf: Int)

case object NoBounds	  extends SliceBounds(0)
case object LowBound    extends SliceBounds(1)
case object HighBound   extends SliceBounds(1)
case object BothBounds  extends SliceBounds(2)
