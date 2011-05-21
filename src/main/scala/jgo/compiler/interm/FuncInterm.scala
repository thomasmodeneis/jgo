package jgo.compiler
package interm

import codeseq._
import instr._
import symbol._
import types._

sealed abstract class FuncInterm extends Typed {
  val target: Func
  val params:  List[LocalVar]
  val results: List[LocalVar]
  val code: Code
  
  lazy val typeOf: FuncType = target.typeOf
  lazy val resultCount: Int = results.length
  def isVariadic: Boolean = typeOf.isVariadic
}

case class FunctionInterm(
    target: Function,
    params:  List[LocalVar],
    results: List[LocalVar],
    code: Code)
extends FuncInterm

case class MethodInterm(
    target: Method,
    receiver: LocalVar,
    params:  List[LocalVar],
    results: List[LocalVar],
    code: Code)
extends FuncInterm

