package jgo.compiler
package interm

import types._

trait Typed {
  val typeOf: Type
  
  final val t: typeOf.type = typeOf //shorthand 
  
  val callable = typeOf.underlying.isInstanceOf[FuncType]
  
  def :=  (otherT: Type): Boolean = typeOf.underlying == otherT
  def :== (otherT: Type): Boolean = typeOf == otherT
}
