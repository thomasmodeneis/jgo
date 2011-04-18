package jgo.compiler
package parser

import interm._
import codeseq._
import instr._
import types._

trait StmtUtils {
  self: Base =>
  
  def badStmt(msg: String, args: AnyRef*): CodeBuilder = {
    recordErr(msg, args: _*)
    CodeBuilder.empty
  }
}
