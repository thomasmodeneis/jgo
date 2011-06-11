package jgo.tools.compiler
package interm
package symbol

import types._
import expr._

case class ConstSymbol(value: ConstExpr) extends ValueSymbol {
  val typeOf = value.typeOf
}
