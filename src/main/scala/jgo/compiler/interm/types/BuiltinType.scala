package jgo.compiler
package interm
package types

sealed trait BuiltinType extends Type with Constable

sealed trait PrimitiveType extends BuiltinType {
  val semantics = Primitive
}

sealed trait AddableType  extends BuiltinType
sealed trait NumericType  extends PrimitiveType with AddableType
sealed trait IntegralType extends NumericType
sealed trait UnsignedType extends IntegralType

case object Bool extends PrimitiveType

case object Uint8  extends UnsignedType
case object Uint16 extends UnsignedType
case object Uint32 extends UnsignedType
case object Uint64 extends UnsignedType

case object Int8  extends IntegralType
case object Int16 extends IntegralType
case object Int32 extends IntegralType
case object Int64 extends IntegralType

case object Float32 extends NumericType
case object Float64 extends NumericType

case object Complex64  extends NumericType //or BuiltinType?
case object Complex128 extends NumericType

sealed trait BuiltinRefType extends Type with Constable {
  val semantics = Reference
}

case object StringType  extends BuiltinRefType with AddableType with Nilable //or Primitive...?

case object TopType     extends BuiltinRefType
case object NilType     extends BuiltinRefType
case object BottomType  extends BuiltinRefType

case object TypeError extends BuiltinRefType {
  //I don't know of any way of overriding hashCode
  //to be consistent with this, so I don't try.
  //Also, this totally breaks the transitivity of ==.
  //This is the conceptually correct behavior, actually!
  //Just don't go putting TypeErrors in a HashSet or something...
  override def equals(other: Any): Boolean = other.isInstanceOf[Type]
}
