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

class TypeTranslator(val pkg: Package, val interm: WrappedType) {
  private val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
  
  private val access =
    if (interm.isPublic)
      ACC_PUBLIC
    else
      0 //Package private.
  
  cw.visit(V1_6, access, pkg.name + "/" + interm.name, null, "java/lang/Object", null)
}
