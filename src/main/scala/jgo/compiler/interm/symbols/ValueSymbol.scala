package jgo.compiler
package interm
package symbols

import interm.types._

trait ValueSymbol extends Symbol {
  val typeOf:   Type
  val callable: Boolean
}
