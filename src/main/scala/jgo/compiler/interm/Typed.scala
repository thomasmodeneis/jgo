package jgo.compiler
package interm

import types._

trait Typed {
  val typeOf: Type
  
  final val t: typeOf.type = typeOf //shorthand 
  
  val callable = typeOf.underlying.isInstanceOf[FuncType]
  
  def isOfType(otherT: Type): Boolean = typeOf.underlying == otherT
  def isOfType[T <: Type] = typeOf.underlying.isInstanceOf[T]
}

object OfType {
  def unapply[T <: Typed](v: T): Option[(T, Type)] =
    Some(v, v.typeOf.underlying)
}

/*
case e OfType t: FuncType

case e OfType (t: FuncType)

case e OfType FuncType(_)

case e |: t: FuncType

case e |: (t: FuncType)

case e |: FuncType(_)

case e =: t: FuncType

case e -: (t: FuncType)
*/
