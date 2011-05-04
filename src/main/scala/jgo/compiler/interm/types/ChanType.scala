package jgo.compiler
package interm
package types

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
extends Type with NilableType {
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
