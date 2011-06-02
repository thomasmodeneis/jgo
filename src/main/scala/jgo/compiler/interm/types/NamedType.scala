package jgo.compiler
package interm
package types

//import member._

sealed trait NamedType extends Type {
  val name: String
  def referent: Type
  override def toString = name
}

class TypeAlias(val name: String, val effective: Type) extends NamedType {
  def underlying = effective.underlying
  def referent   = effective
  
  val semantics = underlying.semantics
}

class WrappedType(val name: String, val unwrapped: Type) extends NamedType {
  def underlying = unwrapped.underlying
  def effective  = this
  def referent   = unwrapped
  
  val semantics = Reference  //!!!
}
