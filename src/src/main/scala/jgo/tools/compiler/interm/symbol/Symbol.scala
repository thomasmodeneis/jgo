package jgo.tools.compiler
package interm
package symbol

import types._

trait Symbol

object Symbol {
  implicit def tname2symbol(nt: NamedType):  TypeSymbol = TypeSymbol(nt)
  implicit def symbol2tname(ts: TypeSymbol): NamedType  = ts.theType
}
