package jgo.compiler
package interm

import types._

trait Typed {
  val typeOf: Type
  
  final def t: typeOf.type = typeOf //shorthand 
  
  def callable = typeOf.underlying.isInstanceOf[FuncType]
  
  def funcType: Option[FuncType] = typeOf.underlying match {
    case ft: FuncType => Some(ft)
    case _            => None
  }
  
  def isOfType(otherT: Type): Boolean =
    typeOf.underlying == otherT
  
  @deprecated("apparently, this doesn't work. too bad.", "May 13, 2011")
  def isOfType[T <: Type] =
    typeOf.underlying.isInstanceOf[T]
}

object OfType {
  def unapply[T <: Typed](v: T): Option[(T, Type)] =
    Some(v, v.typeOf.underlying)
}

object HasType {
  def unapply[T <: Typed](v: T): Option[Type] =
    Some(v.typeOf.underlying)
}
