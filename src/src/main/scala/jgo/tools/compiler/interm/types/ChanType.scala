package jgo.tools.compiler
package interm
package types

import PartialFunction._

object RecvChanType {
  def unapply(t: Type): Option[Type] = t match {
    case ChanType(elemT, true, _) => Some(elemT)
    case _ => None
  }
}

object SendChanType {
  def unapply(t: Type): Option[Type] = t match {
    case ChanType(elemT, _, true) => Some(elemT)
    case _ => None
  }
}

object BidirChanType {
  def unapply(t: Type): Option[Type] = t match {
    case ChanType(elemT, true, true) => Some(elemT)
    case _ => None
  }
}

case class ChanType(elemType: Type, canRecv: Boolean = true, canSend: Boolean = true)
extends UnderType with NilableType {
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

object AnyChanType {
  def unapply(t: Type): Option[Type] = condOpt(t) {
    case ChanType(et, _, _) => et
  }
}
