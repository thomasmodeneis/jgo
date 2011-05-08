package jgo.compiler
package interm
package instr

import types._

object TypeConversions extends TypeConversions

trait TypeConversions {
  implicit def toArith(t: NumericType): Arith = t match {
    case Uint8      => U8
    case Uint16     => U16
    case Uint32     => U32
    case Uint64     => U64
    case Int8       => I8
    case Int16      => I16
    case Int32      => I32
    case Int64      => I64
    case Float32    => F32
    case Float64    => F64
    case Complex64  => C64
    case Complex128 => C128
  }
  implicit def toArith(t: IntegralType): Integral = t match {
    case Uint8      => U8
    case Uint16     => U16
    case Uint32     => U32
    case Uint64     => U64
    case Int8       => I8
    case Int16      => I16
    case Int32      => I32
    case Int64      => I64
  }
  implicit def toArith(t: UnsignedType): Unsigned = t match {
    case Uint8      => U8
    case Uint16     => U16
    case Uint32     => U32
    case Uint64     => U64
  }
  implicit def toArith(t: FloatingType): Floating = t match {
    case Float32    => F32
    case Float64    => F64
  }
}
