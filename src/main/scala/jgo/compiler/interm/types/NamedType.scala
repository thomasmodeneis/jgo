package jgo.compiler
package interm
package types

//import member._

sealed trait NamedType extends Type {
  val name: String
  override def toString = name
}

class TypeAlias(val name: String, override val underlying: Type) extends NamedType {
  val semantics        = underlying.semantics
  override val nilable = underlying.nilable
}


class WrappedType(val name: String, override val underlying: Type) extends NamedType {
  val semantics        = Reference  //!!!
  override val nilable = underlying.nilable
}
