package jgo.tools.compiler
package transl

sealed abstract class RuntimeStackType(val desc: String)

case object BoolT   extends RuntimeStackType("Z")

case object ByteT   extends RuntimeStackType("B")
case object ShortT  extends RuntimeStackType("S")
case object IntT    extends RuntimeStackType("I")
case object LongT   extends RuntimeStackType("J")

case object CharT   extends RuntimeStackType("C")

case object FloatT  extends RuntimeStackType("F")
case object DoubleT extends RuntimeStackType("D")

case object ObjT    extends RuntimeStackType("Ljava/lang/Object;")
