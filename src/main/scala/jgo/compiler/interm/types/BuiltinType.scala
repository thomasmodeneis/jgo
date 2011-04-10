package jgo.compiler
package interm
package types

sealed trait BuiltinType extends Type with Constable

sealed trait PrimitiveType extends BuiltinType {
  val semantics = Primitive
}

case object Bool extends PrimitiveType

case object Int8  extends PrimitiveType
case object Int16 extends PrimitiveType
case object Int32 extends PrimitiveType
case object Int64 extends PrimitiveType

case object Uint8  extends PrimitiveType
case object Uint16 extends PrimitiveType
case object Uint32 extends PrimitiveType
case object Uint64 extends PrimitiveType

case object Float32 extends PrimitiveType
case object Float64 extends PrimitiveType

case object Complex64  extends PrimitiveType //or BuiltinType?
case object Complex128 extends PrimitiveType

case object StringType extends BuiltinType //or Primitive...?
