package jgo.compiler
package interm
package symbol

import types._

final class Function(val name: String, val typeOf: FuncType) extends Func with ValueSymbol
