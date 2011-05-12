package jgo.compiler
package interm
package symbol

import types._

trait Symbol

object Symbol {
  implicit def name2symbol(nt: NamedType):  TypeSymbol = TypeSymbol(nt)
  implicit def symbol2name(ts: TypeSymbol): NamedType  = ts.theType
}
