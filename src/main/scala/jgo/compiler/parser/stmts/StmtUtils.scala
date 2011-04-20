package jgo.compiler
package parser.stmts

import interm._
import codeseq._
import instr._
import types._

trait StmtUtils {
  self: Base =>
  
  def badStmt(msg: String, args: Any*): CodeBuilder = {
    recordErr(msg, args: _*)
    CodeBuilder.empty
  }
  
  def checkArity(left: List[_], right: List[Expr]): Boolean = {
    val (lLen, rLen) = (left length, right length)
    if (lLen != rLen) {
      recordErr("Arity (%d) of left side of assignment unequal to arity (%d) of right side",
        lLen.asInstanceOf[AnyRef],
        rLen.asInstanceOf[AnyRef])
      false
    }
    else
      true
  }
}
