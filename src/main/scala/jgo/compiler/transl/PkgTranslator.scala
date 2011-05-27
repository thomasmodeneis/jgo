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
    val mv0 = cw.visitMethod(access, f.name, methodDesc(f), null, null)
    val mv  = new GeneratorAdapter(mv0, access, f.name, methodDesc(f))
    
    for ((p, i) <- f.paramTypes.zipWithIndex)
      if (p.radix.isInstanceOf[UnsignedType])
        mv.visitParameterAnnotation(i, UnsignedAnnot, true)
    
    mv.visitCode()
    import mv._
    
    val start, end = new AsmLabel
    
    val lbls: mut.Map[Label, AsmLabel] = mut.Map.empty
    implicit def getLbl(l: Label) = lbls.getOrElseUpdate(l, new AsmLabel)
    
    mark(start)
    fInterm.code foreach {
      case PushStr(s)            => push(s)
      case PushInt(l, I64 | U64) => push(l)
      case PushInt(l, _)         => push(l.toInt)
      case PushFloat(d, F64)     => push(d)
      case PushFloat(d, F32)     => push(d.toFloat)
      case PushBool(b)           => push(b)
      case PushNil               => push(null: String) // I think this works
      
      case Pop       => pop()
      case Dup       => dup()
      case Dup_Down1 => dupX1()
      case Dup_Down2 => dupX2()
      case Swap      => swap()
      
      case Duplicate(n) =>
        var i = 0
        while (i < n) {
          dup()
          i += 1
        }
      
      case Lbl(l)  => mark(l)
      case Goto(l) => goTo(l)
      
      case PrintString =>
        getStatic(classOf[System], "out", classOf[java.io.PrintStream])
        swap()
        invokeVirtual(classOf[java.io.PrintStream], new AsmMethod("println", "(Ljava/lang/String;)V"))
      case PrintNumeric(I32) =>
        getStatic(classOf[System], "out", classOf[java.io.PrintStream])
        swap()
        invokeVirtual(classOf[java.io.PrintStream], new AsmMethod("println", "(I)V"))
    }
    //visit parameters and returns here
    mark(end)
    mv.visitEnd()
  }
}
