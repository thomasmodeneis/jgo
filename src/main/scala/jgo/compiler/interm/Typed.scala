package jgo.compiler
package interm

import types._

trait Typed {
  val typeOf: Type
  
  @deprecated("Use typeOf instead.", "May 30, 2011")
  final def t: typeOf.type = typeOf //shorthand
  
  @deprecated("Not a cohesive abstraction.", "May 30, 2011")
  def funcType: Option[FuncType] = typeOf.underlying match {
    case ft: FuncType => Some(ft)
    case _            => None
  }
  
  def isOfType(otherT: Type): Boolean =
    typeOf.underlying == otherT
}

object OfType {
  def unapply[T <: Typed](v: T): Option[(T, Type)] =
    Some(v, v.typeOf.underlying)
}

object HasType {
  def unapply[T <: Typed](v: T): Option[Type] =
    Some(v.typeOf.underlying)
}
