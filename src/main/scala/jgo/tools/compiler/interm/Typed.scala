package jgo.tools.compiler
package interm

import types._

/**
 * A value or item that has a static type.
 */
trait Typed {
  val typeOf: Type
  
  /**
   * The type that should be inferred for variables initialized
   * to this typed value.
   */
  final def inferenceType: Type = typeOf match {
    case u: UntypedConstType => u.default
    case t => t
  }
  
  @deprecated("Use typeOf instead.", "May 30, 2011")
  final def t: typeOf.type = typeOf //shorthand
  
  @deprecated("Not a cohesive abstraction.", "May 30, 2011")
  def funcType: Option[FuncType] = typeOf.underlying match {
    case ft: FuncType => Some(ft)
    case _            => None
  }
  
  /**
   * States whether this typed value has the specified
   * underlying type.
   */
  def isOfType(otherT: Type): Boolean =
    typeOf.underlying == otherT
}

/**
 * Extractor for a typed value and its underlying type.
 */
object OfType {
  def unapply[T <: Typed](v: T): Option[(T, Type)] =
    Some(v, v.typeOf.underlying)
}

/**
 * Extractor for the underlying type of a typed value.
 */
object HasType {
  def unapply[T <: Typed](v: T): Option[Type] =
    Some(v.typeOf.underlying)
}
