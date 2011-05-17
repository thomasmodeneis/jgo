package jgo.compiler
package interm

import codeseq._
import instr._
import symbol._
import types._

abstract class Func extends Typed {
  val typeOf: FuncType
  override val callable = true
}

final class Method(
    val name: String,
    val receiver: WrappedType,
    val params:  List[Type],
    val results: List[Type],
    val isVariadic: Boolean)
extends Func {
  val typeOf = FuncType(receiver :: params, results, isVariadic)
}

case class Lambda(val typeOf: FuncType) extends Func

//final class Closure(override val typeOf: FuncType, val bindings: List[LocalVar], val enclFunc: Func) extends Lambda(typeOf)

//in symbol/Function.scala:
//final class Function(val name: String, val typeOf: FuncType) extends Func with ValueSymbol
