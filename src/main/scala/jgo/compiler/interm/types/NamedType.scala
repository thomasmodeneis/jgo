package jgo.compiler
package interm
package types

//import member._

sealed trait NamedType extends Type {
  val name: String
  override val underlying: Type
  override def toString = name
}

class TypeAlias(val name: String, val underlying: Type) extends NamedType {
  val semantics        = underlying.semantics
  override val nilable = underlying.nilable
}


class WrappedType(val name: String, val underlying: Type) extends NamedType {
  val semantics        = Reference  //!!!
  override val nilable = underlying.nilable
}

/*
class TypeName(val name: String, override val underlying: Type) extends Type with Named {
  override val members = underlying.members
  
  val semantics        = underlying.semantics
  override val nilable = underlying.nilable
  
  override def toString = name
}
*/
