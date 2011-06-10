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

trait Arithmetic extends FuncTranslBase {
  private def not(t: Integral): Unit = t match {
    case I64 | U64 => inst.neg(LONG_TYPE);    inst.lconst(-1); inst.sub(LONG_TYPE)
    case t         => inst.neg(toAsmType(t)); inst.iconst(-1); inst.sub(INT_TYPE)
  }
  
  protected override def translateInstr(i: Instr): Unit = i match {
    case Add(t) => inst.add(toAsmType(t))
    case Sub(t) => inst.sub(toAsmType(t))
    case Mul(t) => inst.mul(toAsmType(t))
    case Div(t) => inst.div(toAsmType(t))
    
    case Mod(t) => inst.rem(toAsmType(t))
    
    case Neg(t)          => inst.neg(toAsmType(t))
    case BitwiseCompl(t) => not(t)
    
    case BitwiseAnd(t: Integral)    => inst.and(toAsmType(t))
    case BitwiseOr(t: Integral)     => inst.or(toAsmType(t))
    case BitwiseAndNot(t: Integral) => not(t); inst.and(toAsmType(t))
    case BitwiseXor(t: Integral)    => inst.xor(toAsmType(t))
    
    case _ => super.translateInstr(i)
  }
}