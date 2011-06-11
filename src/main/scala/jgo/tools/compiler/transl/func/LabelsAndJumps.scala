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
import GeneratorAdapter._

import scala.collection.{mutable => mut}

trait LabelsAndJumps extends FuncTranslBase {
  private val lbls: mut.Map[Label, AsmLabel] = mut.Map.empty
  private implicit def getLbl(l: Label) = lbls.getOrElseUpdate(l, new AsmLabel)
  
  private def negated(mode: Int): Int = mode match {
    case EQ => NE
    case NE => EQ
    case LT => GE
    case LE => GT
    case GT => LE
    case GE => LT
  }
  
  private def numCmpNot(t: StackType, mode: Int, l: AsmLabel): Unit = t match {
    case F32 =>
    case F64 =>
    
    //case C64  =>
    //case C128 =>
    
    case _ => gen.ifCmp(toAsmType(t), negated(mode), l)
  }
  
  protected override def translateInstr(i: Instr): Unit = i match {
    case Lbl(l)  => gen.mark(l)
    case Goto(l) => gen.goTo(l)
    
    case Branch(test, l) => test match {
      case IsTrue => inst.ifne(l) //if not zero
      
      case BoolEq => inst.ificmpeq(l) //int compare-eq
      case BoolNe => inst.ificmpne(l) //int compare-ne
      
      case NumEq(t)  => gen.ifCmp(toAsmType(t), EQ, l)
      case NumNe(t)  => gen.ifCmp(toAsmType(t), NE, l)
      case NumLt(t)  => gen.ifCmp(toAsmType(t), LT, l)
      case NumLeq(t) => gen.ifCmp(toAsmType(t), LE, l)
      case NumGt(t)  => gen.ifCmp(toAsmType(t), GT, l)
      case NumGeq(t) => gen.ifCmp(toAsmType(t), GE, l)
    }
    
    case BranchNot(test, l) => test match {
      case IsTrue => inst.ifeq(l) //if zero
      
      case BoolEq => inst.ificmpne(l) //int compare-ne
      case BoolNe => inst.ificmpeq(l) //int compare-eq
      
      case NumEq(t)  => numCmpNot(t, EQ, l)
      case NumNe(t)  => numCmpNot(t, NE, l)
      case NumLt(t)  => numCmpNot(t, LT, l)
      case NumLeq(t) => numCmpNot(t, LE, l)
      case NumGt(t)  => numCmpNot(t, GT, l)
      case NumGeq(t) => numCmpNot(t, GE, l)
    }
    
    case _ => super.translateInstr(i)
  }
}
