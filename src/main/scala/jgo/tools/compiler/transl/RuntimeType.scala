package jgo.tools.compiler
package transl

sealed abstract class RuntimeType(val desc: String)

case object RuntimeBool extends RuntimeType("Z")

case object RuntimeByte  extends RuntimeType("B")
case object RuntimeShort extends RuntimeType("S")
case object RuntimeInt   extends RuntimeType("I")
case object RuntimeLong  extends RuntimeType("J")

case object RuntimeChar extends RuntimeType("C")

case object RuntimeFloat  extends RuntimeType("F")
case object RuntimeDouble extends RuntimeType("D")

case class RuntimeClass(val internalName: String) extends RuntimeType("L" + internalName + ";")
