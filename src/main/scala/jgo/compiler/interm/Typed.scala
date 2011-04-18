package jgo.compiler
package interm

import types._

trait Typed {
  val typeOf: Type
  
  final val t: typeOf.type = typeOf //shorthand 
  
  val callable = typeOf.underlying.isInstanceOf[FuncType]
  
  def isOfType(otherT: Type): Boolean =
    typeOf.underlying == otherT || (typeOf.underlying eq TypeError) //== would always be true
  
  def isOfType[T <: Type] =
    typeOf.underlying.isInstanceOf[T] || (typeOf.underlying eq TypeError)
}

object OfType {
  def unapply[T <: Typed](v: T): Option[(T, Type)] =
    Some(v, v.typeOf.underlying)
}

object HasType {
  def unapply[T <: Typed](v: T): Option[Type] =
    Some(v.typeOf.underlying)
}
