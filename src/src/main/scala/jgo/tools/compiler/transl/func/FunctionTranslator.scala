package jgo.tools.compiler
package transl
package func

import interm._
import codeseq._
import instr._
import symbol._
import types._

import org.objectweb.asm.MethodVisitor

class FunctionTranslator(
    val pkgName:      String,
    val source:       FunctionInterm,
    protected val mv: MethodVisitor)
  extends FuncTranslBase
  with Arithmetic
  with ArraysAndSlices
  with Strings
  with LabelsAndJumps
  with Returns
  with Locals
  with FuncCalls
  with Prints
  with StackOps {
  
  def isStatic = true
}
