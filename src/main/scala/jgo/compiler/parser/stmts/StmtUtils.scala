package jgo.compiler
package parser
package stmts

import interm._
import expr._
import codeseq._
import instr._
import types._

trait StmtUtils {
  self: Base =>
  
  def checkArity(left: List[_], right: List[Expr]): Boolean = {
    val (lLen, rLen) = (left length, right length)
    if (lLen != rLen) {
      recordErr("Arity (%d) of left side of assignment unequal to arity (%d) of right side", lLen, rLen)
      false
    }
    else
      true
  }
}
