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
import AsmType._

import scala.collection.{mutable => mut}

trait FunctionTranslation extends TypeResolution {
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
      if (p.effective.isInstanceOf[UnsignedType])
        mv.visitParameterAnnotation(i, UnsignedAnnot, true)
    
    mv.visitCode()
    translateCode(fInterm.code, mv)
    mv.visitMaxs(-1, -1) //apparently, we still have to call this, even though the maxes will be computed for us
    mv.visitEnd()
  }
  
  def translateCode(code: Code, mv: GeneratorAdapter) {
    val lbls: mut.Map[Label, AsmLabel] = mut.Map.empty
    implicit def getLbl(l: Label) = lbls.getOrElseUpdate(l, new AsmLabel)
    
    type AsmLocal = Int
    val locals: mut.Map[LocalVar, AsmLocal] = mut.Map.empty
    val localStart: mut.Map[LocalVar, AsmLabel] = mut.Map.empty
    
    val iv = new InstructionAdapter(mv)
    import iv.{mv => _, _}
    
    def not(t: Integral) {
      t match {
        case I64 | U64 => neg(LONG_TYPE);    lconst(-1); sub(LONG_TYPE)
        case t         => neg(toAsmType(t)); iconst(-1); sub(INT_TYPE)
      }
    }
    
    val start, end = new AsmLabel
    mark(start)
    code foreach {
      case PushStr(s)            => mv.push(s)
      case PushInt(i, I64 | U64) => mv.push(i.toLong)
      case PushInt(i, _)         => mv.push(i.toInt)
      case PushFloat(d, F64)     => mv.push(d.toDouble)
      case PushFloat(d, F32)     => mv.push(d.toFloat)
      case PushBool(b)           => mv.push(b)
      case PushNil               => mv.push(null: String) // I think this works
      
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
      
      case Lbl(l)  => mv.mark(l)
      case Goto(l) => goTo(l)
      
      case PrintString =>
        mv.getStatic(classOf[System], "out", classOf[java.io.PrintStream])
        swap()
        mv.invokeVirtual(classOf[java.io.PrintStream], new AsmMethod("println", "(Ljava/lang/String;)V"))
      case PrintNumeric(I32) =>
        mv.getStatic(classOf[System], "out", classOf[java.io.PrintStream])
        swap()
        mv.invokeVirtual(classOf[java.io.PrintStream], new AsmMethod("println", "(I)V"))
      
      case Decl(v) =>
        localStart(v) = mv.mark()
        locals(v) = mv.newLocal(toAsmType(v.typeOf))
      case Undecl(v) =>
        val endLbl = mv.mark()
        mv.visitLocalVariable(v.name, typeDesc(v.typeOf), null, localStart(v), endLbl, locals(v))
        locals -= v //cause error on invalid ref to v
      
      case Add(t) => add(toAsmType(t))
      case Sub(t) => sub(toAsmType(t))
      case Mul(t) => mul(toAsmType(t))
      case Div(t) => div(toAsmType(t))
      
      case Mod(t) => rem(toAsmType(t))
      
      case Neg(t)          => neg(toAsmType(t))
      case BitwiseCompl(t) => not(t)
      
      case BitwiseAnd(t: Integral)    => and(toAsmType(t))
      case BitwiseOr(t: Integral)     => or(toAsmType(t))
      case BitwiseAndNot(t: Integral) => not(t); and(toAsmType(t))
      case BitwiseXor(t: Integral)    => xor(toAsmType(t))
      
    }
    mv.returnValue() //Be sure to refine this to the correct behavior.
    mv.mark(end)
  }
}

