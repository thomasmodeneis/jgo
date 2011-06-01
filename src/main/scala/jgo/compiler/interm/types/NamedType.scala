package jgo.compiler
package interm
package types

//import member._

sealed trait NamedType extends Type {
  val name: String
  override def toString = name
}

class TypeAlias(val name: String, override val effective: Type) extends NamedType {
  override def underlying = effective.underlying
  override def nilable    = underlying.nilable
  
  val semantics = underlying.semantics
}


class WrappedType(val name: String, val unwrapped: Type) extends NamedType {
  override def underlying = unwrapped.underlying
  override def nilable    = underlying.nilable
  
  val semantics = Reference  //!!!
}
