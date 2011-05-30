package jgo.compiler
package transl

import interm._
import codeseq._
import instr._
import symbol._
import types._
import interm.Label
import interm.types.Type

import RuntimeInfo._

import org.objectweb.asm
import asm.{ClassWriter, Label => AsmLabel, Type => AsmType}
import asm.commons.{GeneratorAdapter, InstructionAdapter, Method => AsmMethod}
import asm.Opcodes._

import scala.collection.{mutable => mut}

class PkgTranslator(val interm: PkgInterm) extends TypeResolution with FunctionTranslation {
  val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
  
  cw.visit(V1_6, ACC_PUBLIC, interm.target.name, null, "java/lang/Object", null)
  
  interm.globals foreach { global =>
    val access =
      if (global.isPublic)
        ACC_PUBLIC | ACC_STATIC
      else
        ACC_STATIC //0 = package private
    val fieldVis = cw.visitField(access, global.name, typeDesc(global.typeOf), null, null)
    if (global.typeOf.radix.isInstanceOf[UnsignedType])
      fieldVis.visitAnnotation(UnsignedAnnot, true)
    fieldVis.visitEnd()
  }
  
  /*{
    val mv0 = cw.visitMethod(ACC_PUBLIC | ACC_STATIC, "<clinit>", "()V", null, null)
    val mv  = new GeneratorAdapter(mv0, ACC_PUBLIC | ACC_STATIC, "<clinit>", "()V")
    translateCode(interm.initCode, mv)
  }*/
  
  interm.functions foreach { case (f, fInterm) =>
    translateFunction(fInterm, cw)
  }
  
  cw.visitEnd()
  
  def outputBytes = cw.toByteArray()
}
