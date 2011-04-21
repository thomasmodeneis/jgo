package jgo.compiler
package interm

import instr._

package object codeseq {
  implicit def fromInstr(instr: Instr): CodeBuilder = {
    val ls = Code(instr)
    new CodeBuilder(ls, ls)
  }
  
  implicit def fromOption(opt: Option[CodeBuilder]): CodeBuilder =
    opt getOrElse CodeBuilder()
}
