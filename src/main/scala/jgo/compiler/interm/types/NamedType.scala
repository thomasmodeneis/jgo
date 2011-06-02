package jgo.compiler
package interm
package types

//import member._

sealed trait NamedType extends Type {
  val name: String
  def referent: Type
  override def toString = name
}

class TypeAlias(val name: String, val referent: Type) extends NamedType {
  def underlying = referent.underlying
  def effective  = referent
  
  val semantics = underlying.semantics
}

class WrappedType(val name: String, val referent: Type) extends NamedType {
  def underlying = referent.underlying
  def effective  = this
  
  val semantics = Reference  //!!!
}
