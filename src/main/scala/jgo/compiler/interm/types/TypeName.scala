package jgo.compiler
package interm
package types

import symbols.Symbol

class TypeName(val name: String, val underlying: Type) extends Type with Symbol {
  def semantics = underlying.semantics
  
  def nilable   = underlying.nilable
  
  def canEqual(that: Any): Boolean =
    that.isInstanceOf[TypeName]
}
