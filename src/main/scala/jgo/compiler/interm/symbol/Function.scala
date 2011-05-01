package jgo.compiler
package interm
package symbol

import types._

class Function(val name: String, val typeOf: FuncType) extends ValueSymbol {
  override val callable = true
}
