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
import asm.commons.{Method => AsmMethod}
import asm.Opcodes._
import AsmType._

//ALERT:  Strings behave like Java UTF-16 strings, not like
//proper Go UTF-8 strings.  Sorry.  :(
//I plan to address this at some point in the future.  Low priority.
//Q:  Why are really important things labeled "low priority"?
//A:  Because there are even more important things (structs, packages, interfaces)
//that should be done first.  I think this is reasonable.  All TODOs, ALERTs, and
//CONCERNs will be addressed in due time.  Thanks for understanding.
trait Strings extends FuncTranslBase {
  private val StringT = AsmType.getObjectType("java/lang/String")
  private val CharAt = new AsmMethod("charAt", "(I)C")
  private val Length = new AsmMethod("length", "()I")
  private val Concat = new AsmMethod("concat", "(Ljava/lang/String;)Ljava/lang/String;")
  private val SubstringM = new AsmMethod("substring", "(II)Ljava/lang/String;")
  private val SubstringMLow = new AsmMethod("substring", "(I)Ljava/lang/String;")
  
  protected override def translateInstr(i: Instr): Unit = i match {
    case StrIndex(U64 | I64) => throw new UnsupportedInstrException(i)
    case StrIndex(_) => gen.invokeVirtual(StringT, CharAt)
    
    case StringLen => gen.invokeVirtual(StringT, Length)
    
    case StrAdd => gen.invokeVirtual(StringT, Concat)
    
    //CONCERN:  What if the bounds that are pushed are of type I64?  I think will fail verification.
    case Substring(bounds) => bounds match {
      case NoBounds   =>
      case LowBound   => gen.invokeVirtual(StringT, SubstringMLow)
      case HighBound  => gen.push(0); gen.swap(); gen.invokeVirtual(StringT, SubstringM)
      case BothBounds => gen.invokeVirtual(StringT, SubstringM)
    }
    
    case _ => super.translateInstr(i)
  }
}