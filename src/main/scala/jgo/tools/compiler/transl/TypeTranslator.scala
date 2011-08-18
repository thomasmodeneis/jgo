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
import asm.{ClassWriter, FieldVisitor, MethodVisitor, Label => AsmLabel, Type => AsmType}
import asm.commons.{GeneratorAdapter, InstructionAdapter, Method => AsmMethod}
import asm.Opcodes._

import scala.collection.{mutable => mut}

//TODO: Package info should be contained in WrappedType
class TypeTranslator(val pkg: Package, val interm: WrappedType) extends TypeResolution with GoSignatures {
  private val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
  
  private def access(isPublic: Boolean) =
    if (isPublic)
      ACC_PUBLIC
    else
      0 //Package private.
  
  private val internalName = pkg.name + "/" + interm.name
  
  cw.visit(V1_6, access(interm.isPublic) | ACC_FINAL, internalName, null, "java/lang/Object", null)
  
  interm.referent match {
    case StructType(fields) =>
      fields foreach { f =>
        val fieldVis = cw.visitField(access(f.isPublic), f.name, typeDesc(f.typeOf), null, null)
        if (f.typeOf.effective.isInstanceOf[UnsignedType])
          fieldVis.visitAnnotation(UnsignedAnnot.Class, true)
        val sigAnnot = fieldVis.visitAnnotation(GoTypeAnnot.Class, true)
        sigAnnot.visit("value", typeSig(f.typeOf))
        sigAnnot.visitEnd()
        fieldVis.visitEnd()
      }
    
    case otherT =>
      val otherTDesc = typeDesc(otherT)
      val fieldVis = cw.visitField(ACC_PUBLIC | ACC_FINAL, "value", otherTDesc, null, null)
      if (otherT.effective.isInstanceOf[UnsignedType])
        fieldVis.visitAnnotation(UnsignedAnnot.Class, true)
      val sigAnnot = fieldVis.visitAnnotation(GoTypeAnnot.Class, true)
      sigAnnot.visit("value", typeSig(otherT))
      sigAnnot.visitEnd()
      fieldVis.visitEnd()
      
      //Create a constructor.
      val constrVis = cw.visitMethod(ACC_PUBLIC, "<init>", "(" + otherTDesc + ")V", null, null)
      constrVis.visitCode()
      constrVis.visitVarInsn(ALOAD, 0)
      constrVis.visitVarInsn(ALOAD, 1)
      constrVis.visitFieldInsn(PUTFIELD, internalName, "value", otherTDesc)
      constrVis.visitInsn(RETURN)
      //Apparently, we still have to call this,
      //even though the maxes will be computed for us.
      constrVis.visitMaxs(-1, -1)
      constrVis.visitEnd()
      
      //Create a factory method `of`.
      val factoryVis = cw.visitMethod(ACC_PUBLIC | ACC_STATIC, "of", "(" + otherTDesc + ")L" + internalName + ";", null, null)
      factoryVis.visitCode()
      factoryVis.visitTypeInsn(NEW, internalName)
      factoryVis.visitInsn(DUP)
      factoryVis.visitMethodInsn(INVOKESPECIAL, internalName, "<init>", "(" + otherTDesc + ")V")
      factoryVis.visitInsn(ARETURN)
      //Apparently, we still have to call this,
      //even though the maxes will be computed for us.
      constrVis.visitMaxs(-1, -1)
      constrVis.visitEnd()
  }
  
  cw.visitEnd()
  
  def outputBytes = cw.toByteArray()
}
