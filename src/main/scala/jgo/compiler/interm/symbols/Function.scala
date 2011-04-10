package jgo.compiler
package interm
package symbols

import types._

class Function(val name: String, val typeOf: Type) extends ValueSymbol {
  require(typeOf.isInstanceOf[FuncType])
  val callable = true
}
