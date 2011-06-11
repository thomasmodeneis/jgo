package jgo.tools.compiler
package transl
package func

import org.objectweb.asm
import asm.{MethodVisitor, Type => AsmType}
import asm.commons.{GeneratorAdapter, InstructionAdapter, Method => AsmMethod}
import asm.Opcodes._

import scala.collection.mutable.ArrayBuffer

private object NonStupidGeneratorAdapter {
  def access(isStatic: Boolean) =
    if (isStatic) ACC_STATIC
    else 0
  
  def name = "~ name not actually needed ~"
}
import NonStupidGeneratorAdapter._


private class NonStupidGeneratorAdapter(mv: MethodVisitor, desc: String, isStatic: Boolean)
extends GeneratorAdapter(mv, access(isStatic), name, desc) {
  
  private val argTypes = AsmType.getArgumentTypes(desc)
  
  //in part based on GeneratorAdapter.getArgIndex
  //this will allow O(1) getArgIndex vs GeneratorAdapter's O(n)
  private val argIndices = {
    val indices = new ArrayBuffer[Int](argTypes.length)
    var curIndex = if (isStatic) 0 else 1 //if not static, skip `this`
    for (arg <- argTypes) {
      indices += curIndex
      curIndex += arg.getSize
    }
    indices.readOnly
  }
  
  private val argTypeByIndex: Map[Int, AsmType] =
    Map((argIndices zip argTypes): _*)
  
  /**
   * Returns the type of the specified local variable, which, unlike in `GeneratorAdapter`,
   * ''may'' be a formal parameter.  This modification resolves a major issue with
   * `GeneratorAdapter`.
   */
  override def getLocalType(local: Int): AsmType =
    if (local < firstLocal)
      argTypeByIndex(local)
    else
      super.getLocalType(local)
}
