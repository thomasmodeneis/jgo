package jgo.compiler
package interm
package types

case class PointerType(elemType: Type) extends Type with Nilable {
  override val isInterface = elemType.isInterface
  val          semantics   = Reference
  
  override def toString = "*" + elemType
}

case class ArrayType(length: Int, elemType: Type) extends Type {
  val semantics = Value
  
  override def toString = "[" + length + "]" + elemType
}

case class SliceType(elemType: Type) extends Type with Nilable {
  val semantics = Reference
  
  override def toString = "[]" + elemType
}

case class MapType(keyType: Type, valueType: Type) extends Type with Nilable {
  val semantics = Reference
  
  override def toString = "map[" + keyType + "]" + valueType
}
