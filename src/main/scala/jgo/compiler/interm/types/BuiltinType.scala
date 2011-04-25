package jgo.compiler
package interm
package types

sealed trait BuiltinType extends Type with Constable

sealed trait PrimitiveType extends BuiltinType with Named {
  val semantics = Primitive
}

sealed trait AddableType  extends BuiltinType
sealed trait NumericType  extends PrimitiveType with AddableType
sealed trait IntegralType extends NumericType
sealed trait UnsignedType extends IntegralType

case object BoolType extends PrimitiveType  { val name = "bool" }

case object Uint8  extends UnsignedType { val name = "uint8" }
case object Uint16 extends UnsignedType { val name = "uint16" }
case object Uint32 extends UnsignedType { val name = "uint32" }
case object Uint64 extends UnsignedType { val name = "uint64" }

case object Int8  extends IntegralType { val name = "int8" }
case object Int16 extends IntegralType { val name = "int16" }
case object Int32 extends IntegralType { val name = "int32" }
case object Int64 extends IntegralType { val name = "int64" }

case object Float32 extends NumericType { val name = "float32" }
case object Float64 extends NumericType { val name = "float64" }

case object Complex64  extends NumericType { val name = "complex64" } //or BuiltinType?
case object Complex128 extends NumericType { val name = "complex128" }

sealed trait BuiltinRefType extends Type with Constable {
  val semantics = Reference
}

case object StringType extends BuiltinRefType //or Primitive...?
                          with AddableType
                          with Nilable
                          with Named {
  val name = "string"
}

case object TopType     extends BuiltinRefType
case object NilType     extends BuiltinRefType
case object BottomType  extends BuiltinRefType
case object UnitType    extends BuiltinRefType

case object TypeError extends BuiltinRefType with Named {
  //I don't know of any way of overriding hashCode
  //to be consistent with this, so I don't try.
  //Also, this totally breaks the transitivity of ==.
  //This is the conceptually correct behavior, actually!
  //Just don't go putting TypeErrors in a HashSet or something...
  //override def equals(other: Any): Boolean = other.isInstanceOf[Type]
  val name = "<error>"
}
