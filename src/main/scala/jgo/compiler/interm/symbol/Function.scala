package jgo.compiler
package interm
package symbol

import types._

class Function(val name: String, val typeOf: Type) extends ValueSymbol {
  require(typeOf.isInstanceOf[FuncType])
  override val callable = true
}
