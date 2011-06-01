package jgo.compiler
package interm
package instr

sealed abstract class StackType

case object Obj extends StackType

sealed abstract class Prim extends StackType

case object Bool extends Prim

sealed trait Arith    extends Prim
sealed trait Complex  extends Arith
sealed trait Real     extends Arith
sealed trait Floating extends Real
sealed trait Integral extends Real
sealed trait Unsigned extends Integral

case object I8  extends Integral
case object I16 extends Integral
case object I32 extends Integral
case object I64 extends Integral

case object U8  extends Unsigned
case object U16 extends Unsigned
case object U32 extends Unsigned
case object U64 extends Unsigned

case object F32 extends Floating
case object F64 extends Floating

case object C64  extends Complex
case object C128 extends Complex
