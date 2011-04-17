package jgo.compiler
package interm
package types

import member._

class TypeName(val name: String, override val underlying: Type) extends Type with Named {
  override val members = underlying.members
  
  val semantics        = underlying.semantics
  override val nilable = underlying.nilable
  
  def canEqual(that: Any): Boolean =
    that.isInstanceOf[TypeName]
}
