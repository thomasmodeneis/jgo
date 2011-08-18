package jgo.tools.compiler
package transl

import func.FunctionTranslator

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

class PkgTranslator(val interm: PkgInterm) extends TypeResolution with GoSignatures {
  //ClassWriter for the package's class file.  (package.class)
  private val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
  
  cw.visit(V1_6, ACC_PUBLIC, interm.target.name + "/package", null, "java/lang/Object", null)
  
  interm.globals foreach { global =>
    val access =
      if (global.isPublic)
        ACC_PUBLIC | ACC_STATIC
      else
        ACC_STATIC //0 = package private
    val fieldVis = cw.visitField(access, global.name, typeDesc(global.typeOf), null, null)
    if (global.typeOf.effective.isInstanceOf[UnsignedType])
      fieldVis.visitAnnotation(UnsignedAnnot.Class, true)
    val sigAnnot = fieldVis.visitAnnotation(GoTypeAnnot.Class, true)
    sigAnnot.visit("value", typeSig(global.typeOf))
    sigAnnot.visitEnd()
    fieldVis.visitEnd()
  }
  
  /*{
    val mv0 = cw.visitMethod(ACC_PUBLIC | ACC_STATIC, "<clinit>", "()V", null, null)
    val mv  = new GeneratorAdapter(mv0, ACC_PUBLIC | ACC_STATIC, "<clinit>", "()V")
    translateCode(interm.initCode, mv)
  }*/
  
  interm.functions foreach { case (f, fInterm) =>
    val access =
      if (f.isPublic || f.name == "main") //FIXME: This is a really big hack. Improve the logic here.
        ACC_PUBLIC | ACC_STATIC
      else
        ACC_STATIC //0 = package private
    
    val mv = cw.visitMethod(access, f.name, methodDesc(f), null, null)
    
    new FunctionTranslator(fInterm, mv).translate()
  }
  
  cw.visitEnd()
  
  def outputBytes = cw.toByteArray()
}
