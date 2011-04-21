package jgo.compiler
package interm.instr

sealed abstract class SliceBounds(val numberOf: Int)
case object NoBound 	  extends SliceBounds(0)
case object LowBound    extends SliceBounds(1)
case object HighBound   extends SliceBounds(1)
case object BothBounds  extends SliceBounds(2)
