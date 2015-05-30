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

trait StackOps extends FuncTranslBase {
  protected override def translateInstr(i: Instr): Unit = i match {
    case PushStr(s)            => gen.push(s)
    case PushInt(i, I64 | U64) => gen.push(i.toLong)
    case PushInt(i, _)         => gen.push(i.toInt)
    case PushFloat(d, F64)     => gen.push(d.toDouble)
    case PushFloat(d, F32)     => gen.push(d.toFloat)
    case PushBool(b)           => gen.push(b)
    case PushNil               => gen.push(null: String) // I think this works
    
    case Pop       => inst.pop()
    case Dup       => inst.dup()
    case Dup_Down1 => inst.dupX1()
    case Dup_Down2 => inst.dupX2()
    case Swap      => inst.swap()
    
    //Duplicate(1) is equiv to Dup
    case Duplicate(n) =>
      var i = 0
      while (i < n) {
        inst.dup()
        i += 1
      }
    
    case _ => super.translateInstr(i)
  }
}