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

import scala.collection.{mutable => mut}

trait Locals extends FuncTranslBase {
  private type AsmLocal = Int
  
  private val locals:     mut.Map[LocalVar, AsmLocal] = mut.Map.empty
  private val localStart: mut.Map[LocalVar, AsmLabel] = mut.Map.empty
  
  private val start: AsmLabel = new AsmLabel
  
  protected override def beforeInstrs() {
    assert(signature.namedParams.length == 0 ||
           signature.namedParams.length == signature.paramTypes.length)
    
    gen.mark(start)
    
    var curLocal = 0
    for ((v, t) <- signature.namedParams zip signature.paramTypes) {
      assert(v.typeOf == t)
      locals(v) = curLocal
      curLocal += stackSize(t)
    }
    
    for (v <- signature.namedResults)
      locals(v) = gen.newLocal(toAsmType(v.typeOf))
    
    super.beforeInstrs()
  }
  
  protected override def translateInstr(i: Instr): Unit = i match {
    case Decl(v) =>
      localStart(v) = gen.mark()
      locals(v) = gen.newLocal(toAsmType(v.typeOf))
    
    case Undecl(v) =>
      val endLbl = gen.mark()
      gen.visitLocalVariable(v.name, typeDesc(v.typeOf), null, localStart(v), endLbl, locals(v))
      locals -= v //cause error on invalid ref to v
    
    case LoadVar(v: LocalVar)  => gen.loadLocal(locals(v))
    case StoreVar(v: LocalVar) => gen.storeLocal(locals(v))
    
    case Incr(v: LocalVar, n, t) => gen.iinc(locals(v), n)
    case Decr(v: LocalVar, n, t) => gen.iinc(locals(v), -n)
    
    case _ => super.translateInstr(i)
  }
  
  protected override def afterInstrs() {
    super.afterInstrs()
    //locals should contain only func-wide variables (params, results, defer stack) now
    val end = gen.mark()
    for ((v, l) <- locals)
      gen.visitLocalVariable(v.name, typeDesc(v.typeOf), null, start, end, l)
  }
}