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

object RecvChanType {
  def unapply(t: Type): Option[Type] = t match {
    case ChanType(et, true, _) => Some(et)
    case _ => None
  }
}
object SendChanType {
  def unapply(t: Type): Option[Type] = t match {
    case ChanType(et, _, true) => Some(et)
    case _ => None
  }
}
case class ChanType(elemType: Type, canRecv: Boolean = true, canSend: Boolean = true)
extends Type with Nilable {
  require(canRecv || canSend, "implementation error: ChanType that can neither send nor receive created")
  
  val semantics = Reference
  
  override def toString = {
    if (canRecv && canSend)
      "chan "
    else if (canRecv)
      "<-chan "
    else if (canSend)
      "chan<- "
    else
      throw new AssertionError("Somehow, a ChanType(_, false, false) slipped through the cracks")
  } + elemType
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
  
  override def toString =
    if (!isVariadic)
      "func(" + params.mkString(", ") + ") " + results.mkString(", ")
    else
      //ugh...
      "func(" + params.init.mkString(", ") + ", ..." + params.tail + ") " + results.mkString(", ")
}
