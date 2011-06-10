package jgo.compiler
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

trait FuncCalls extends FuncTranslBase {
  protected override def translateInstr(i: Instr): Unit = i match {
    case InvokeFunction(f) =>
      val pkgClass = AsmType.getObjectType("package")
      val method = new AsmMethod(f.name, methodDesc(f))
      gen.invokeStatic(pkgClass, method)
    
    case _ => super.translateInstr(i)
  }
}
