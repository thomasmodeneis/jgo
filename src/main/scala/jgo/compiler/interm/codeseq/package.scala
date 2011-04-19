package jgo.compiler
package interm

import instr._

package object codeseq {
  implicit def fromInstr(instr: Instr): CodeBuilder = {
    val ls = Code(instr)
    new CodeBuilder(ls, ls)
  }
}
