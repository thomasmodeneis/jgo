package jgo.compiler
package transl

import interm._
import codeseq._
import instr._
import symbol._
import types._

import org.objectweb.asm._
import commons._
import Opcodes._

class PkgTranslator(val interm: PkgInterm) {
  val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
  cw.visit(V1_6)
  globals foreach { global =>
    global 
  }
}
