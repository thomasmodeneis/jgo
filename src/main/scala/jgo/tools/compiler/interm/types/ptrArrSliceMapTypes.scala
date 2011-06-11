package jgo.tools.compiler
package interm
package types

case class PointerType(elemType: Type) extends UnderType with NilableType {
  override val isInterface = elemType.isInterface
  val          semantics   = Reference
  
  override def toString = "*" + elemType
}

case class ArrayType(length: Int, elemType: Type) extends UnderType {
  val semantics = Value
  
  override def toString = "[" + length + "]" + elemType
}

case class SliceType(elemType: Type) extends UnderType with NilableType {
  val semantics = Reference
  
  override def toString = "[]" + elemType
}

case class MapType(keyType: Type, valueType: Type) extends UnderType with NilableType {
  val semantics = Reference
  
  override def toString = "map[" + keyType + "]" + valueType
}
