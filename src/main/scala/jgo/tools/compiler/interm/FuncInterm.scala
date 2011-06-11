package jgo.tools.compiler
package interm

import codeseq._
import instr._
import symbol._
import types._

/**
 * The intermediate representation of a certain func.
 */
sealed abstract class FuncInterm extends Typed {
  val target: Func
  val sig: Signature
  val code: Code
  
  lazy val typeOf: FuncType = target.typeOf
  def isVariadic: Boolean = typeOf.isVariadic
}

/**
 * The intermediate representation of a certain (top-level) function.
 */
case class FunctionInterm(
  target: Function,
  sig: Signature,
  code: Code)
extends FuncInterm

/**
 * The intermediate representation of a certain method.
 */
case class MethodInterm(
  target: Method,
  receiver: ParamVar,
  sig: Signature,
  code: Code)
extends FuncInterm

