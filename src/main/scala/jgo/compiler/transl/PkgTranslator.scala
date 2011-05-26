package jgo.compiler
package transl

import interm._
import codeseq._
import instr._
import symbol._
import types._

import RuntimeInfo._

import org.objectweb.asm._
import org.objectweb.asm.{Label => AsmLabel}
import commons._
import Opcodes._

class PkgTranslator(val interm: PkgInterm) extends TypeTranslation {
  val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
  
  cw.visit(V1_6, ACC_PUBLIC, interm.target.name, null, "java/lang/Object", null)
  
  interm.globals foreach { global =>
    val access = if (global.isPublic) ACC_PUBLIC else 0 //0 = package private
    val fieldVis = cw.visitField(access, global.name, typeDesc(global.t), null, null)
    if (global.t.radix.isInstanceOf[UnsignedType])
      fieldVis.visitAnnotation(UnsignedAnnot, true)
    fieldVis.visitEnd()
  }
  
  interm.functions foreach { case (f, fInterm) =>
    val access = if (f.isPublic) ACC_PUBLIC else 0 //0 = package private
    val mv = cw.visitMethod(access, f.name, methodDesc(f), null, null)
    
    for ((p, i) <- f.paramTypes.zipWithIndex)
      if (p.radix.isInstanceOf[UnsignedType])
        mv.visitParameterAnnotation(i, UnsignedAnnot, true)
    
    mv.visitCode()
    val start, end = new AsmLabel
    
    val iv = new InstructionAdapter(mv)
    
    fInterm.code foreach {
      
    }
    //visit parameters and returns here
  }
}
