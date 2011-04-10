package jgo.compiler
package interm
package symbols

import types._

case class TypeSymbol(typeName: TypeName) extends Symbol

object TypeSymbol {
  implicit def name2symbol(tn: TypeName): TypeSymbol = TypeSymbol(tn)
  implicit def symbol2name(ts: TypeSymbol): TypeName = ts.typeName
}
