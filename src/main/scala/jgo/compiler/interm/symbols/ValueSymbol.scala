package jgo.compiler
package interm
package symbols

import types._

trait ValueSymbol extends Symbol {
  val typeOf:   Type
  val callable: Boolean = typeOf.underlying.isInstanceOf[FuncType]
}
