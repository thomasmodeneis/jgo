package jgo.compiler
package interm
package types

case class PointerType(elemType: Type) extends Type with Nilable {
  override val isInterface = elemType.isInterface
  val          semantics   = Reference
}

case class ArrayType(length: Int, elemType: Type) extends Type {
  val semantics = Value
}

case class SliceType(elemType: Type) extends Type with Nilable {
  val semantics = Reference
}

case class MapType(keyType: Type, valueType: Type) extends Type with Nilable {
  val semantics = Reference
}

case class ChanType(elemType: Type, canReceive: Boolean = true, canSend: Boolean = true)
extends Type with Nilable {
  val semantics = Reference
}

case class FuncType(params: List[Type], results: List[Type], isVariadic: Boolean)
extends Type with Nilable {
  /**
   * not sure what is existentially appropriate here,
   * but this has the correct codegen implications because
   * copy constructor should should not be called on
   * assignment of function values.
   */
  val semantics = Reference
}
