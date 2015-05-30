package jgo.tools.compiler
package interm
package symbol

import expr.bfunc.BuiltinFunc

case class BuiltinFuncSymbol(bf: BuiltinFunc) extends Symbol
