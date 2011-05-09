package jgo.compiler
package interm

import types._

package object symbol {
  implicit def name2symbol(nt: NamedType):  TypeSymbol = TypeSymbol(nt)
  implicit def symbol2name(ts: TypeSymbol): NamedType  = ts.theType
}
