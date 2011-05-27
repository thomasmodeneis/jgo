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
import asm.{ClassWriter, ClassVisitor, MethodVisitor, Label => AsmLabel, Type => AsmType}
import asm.commons.{GeneratorAdapter, InstructionAdapter, Method => AsmMethod}
import asm.Opcodes._

import scala.collection.{mutable => mut}

trait FunctionTranslation extends TypeTranslation {
  def translateFunction(fInterm: FunctionInterm, cw: ClassVisitor) {
    val f = fInterm.target
    println("dealing with function " + f.name + ": " + f.typeOf)
    
    val access =
      if (f.isPublic || f.name == "main") //this is a really big hack. improve the logic here.
        ACC_PUBLIC | ACC_STATIC
      else
        ACC_STATIC //0 = package private
    
    val mv0 = cw.visitMethod(access, f.name, methodDesc(f), null, null)
    val mv  = new GeneratorAdapter(mv0, access, f.name, methodDesc(f))
    
    for ((p, i) <- f.paramTypes.zipWithIndex)
      if (p.radix.isInstanceOf[UnsignedType])
        mv.visitParameterAnnotation(i, UnsignedAnnot, true)
    
    translateCode(fInterm.code, mv)
  }
  
  def translateCode(code: Code, mv: GeneratorAdapter) {
    val lbls: mut.Map[Label, AsmLabel] = mut.Map.empty
    implicit def getLbl(l: Label) = lbls.getOrElseUpdate(l, new AsmLabel)
    
    import mv._
    
    visitCode()
    val start, end = new AsmLabel
    mark(start)
    code foreach {
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
    returnValue() //Be sure to refine this to the correct behavior.
    mark(end)
    visitMaxs(-1, -1) //apparently, we still have to call this, even though the maxes will be computed for us
    visitEnd()
  }
}

