package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

private sealed abstract class ConstExpr {
}

private case class IntConstExpr(value: BigInt, typeOf: IntegralType) extends ConstExpr {
  def eval = IntConst(value.toLong, typeOf)
}
