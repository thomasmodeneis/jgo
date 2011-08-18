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
import asm.MethodVisitor
import asm.commons.{GeneratorAdapter, InstructionAdapter, Method => AsmMethod}
import asm.Opcodes._

/**
 * Base trait for func translators.  A func translator is an object that
 * translates the intermediate form of a certain func into bytecode.
 * 
 * Not thread-safe.
 */
trait FuncTranslBase extends TypeResolution with GoSignatures {
  /**
   * The intermediate form that is to be translated by this func translator,
   * which we refer to as the ''source'' of this translator.
   */
  def source: FuncInterm
  
  /**
   * The ''target func'' of this translator; that is, the func whose intermediate
   * form this translator translates
   */
  def target: Func = source.target
  
  /**
   * The signature of the source intermediate form of this translator;
   * this value indicates (for example) whether or not the source has
   * named result variables.
   */
  def signature: Signature = source.sig
  
  
  
  def isStatic: Boolean
  
  /**
   * The method visitor that this translator should write its func to.
   * We call this visitor the ''output visitor'' of this func translator.
   */
  protected def mv: MethodVisitor
  
  /**
   * A GeneratorAdapter to aid in the generation of bytecode.
   * Note that this field may be used in tandem with `inst`.
   */
  protected val gen: GeneratorAdapter =
    new NonStupidGeneratorAdapter(mv, methodDesc(target), isStatic)
  
  /**
   * An InstructionAdapter to aid in the generation of bytecode.
   * Note that this field may be used in tandem with  `gen`.
   */
  protected val inst = new InstructionAdapter(gen)
  
  
  private var translated = false
  
  /**
   * Translates this translator's func into bytecode, "writing" (visiting)
   * the output to this translator's output visitor.
   */
  def translate() {
    if (translated)
      throw new AlreadyTranslatedException
    
    annotations()
    paramAnnotations()
    translateCode()
    mv.visitEnd()
    translated = true
  }
  
  protected def annotations() {
    val sigAnnot = mv.visitAnnotation(GoTypeAnnot.Class, true)
    sigAnnot.visit("value", typeSig(target.typeOf))
    sigAnnot.visitEnd()
  }
  
  protected def paramAnnotations() {
    for ((p, i) <- target.paramTypes.zipWithIndex)
      if (p.effective.isInstanceOf[UnsignedType])
        mv.visitParameterAnnotation(i, UnsignedAnnot.Class, true).visitEnd()
  }
  
  private def translateCode() {
    mv.visitCode()
    beforeInstrs()
    source.code foreach { i =>
      println(i)
      try translateInstr(i)
      catch {
        case e: UnsupportedInstrException => throw e
        case e => e.printStackTrace(); throw AtInstrException(i, e)
      }
    }
    println()
    println()
    if (target.resultTypes == Nil)
      gen.returnValue() //prevent "falling off code".  HACK!!
    afterInstrs()
    //Apparently, we still have to call this,
    //even though the maxes will be computed for us.
    mv.visitMaxs(-1, -1)
  }
  
  protected def beforeInstrs() { }
  protected def afterInstrs() { }
  
  /**
   * Translates the specified instruction.
   * 
   * Subtraits override this method to add support for the instructions
   * they implement, passing those they do not to `super.translateInstr`.
   * 
   * This implementation throws an UnsupportedInstrException.
   */
  protected def translateInstr(i: Instr): Unit =
    throw new UnsupportedInstrException(i)
}
