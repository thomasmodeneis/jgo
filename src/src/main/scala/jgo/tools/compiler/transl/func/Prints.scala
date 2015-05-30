package jgo.tools.compiler
package transl
package func

import interm._
import codeseq._
import instr._
import symbol._
import types._
import interm.Label
import interm.types.Type

import RuntimeInfo._

import org.objectweb.asm
import asm.{ClassWriter, ClassVisitor, MethodVisitor, Label => AsmLabel, Type => AsmType}
import asm.commons.{GeneratorAdapter, InstructionAdapter, Method => AsmMethod}
import asm.Opcodes._
import AsmType._

trait Prints extends FuncTranslBase {
  protected override def translateInstr(i: Instr): Unit = i match {
    case PrintString =>
      gen.getStatic(classOf[System], "out", classOf[java.io.PrintStream])
      inst.swap()
      gen.invokeVirtual(classOf[java.io.PrintStream], new AsmMethod("println", "(Ljava/lang/String;)V"))
    case PrintNumeric(I32) =>
      gen.getStatic(classOf[System], "out", classOf[java.io.PrintStream])
      inst.swap()
      gen.invokeVirtual(classOf[java.io.PrintStream], new AsmMethod("println", "(I)V"))
    
    case _ => super.translateInstr(i)
  }
}