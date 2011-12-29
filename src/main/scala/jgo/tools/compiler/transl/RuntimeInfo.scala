package jgo.tools.compiler
package transl

import interm._
import codeseq._
import instr._
import symbol._
import types._

import org.objectweb.asm
import org.objectweb.asm.commons.{Method => AsmMethod}

object RuntimeInfo {
  type PkgLoc = String
  
  val RuntimePkg:      PkgLoc = "jgo/runtime/"
  val RuntimeAnnotPkg: PkgLoc = "jgo/runtime/annot/"
  
  trait RuntimeClass {
    val Pkg:  PkgLoc
    val Name: String
    
    lazy val Class   = Pkg + Name
    lazy val Desc    = "L%s%s;" format (Pkg, Name)
    lazy val AsmType = asm.Type.getObjectType(Class)
    
    /** Template for the set of instance methods on this runtime class. */
    //trait Methods
    /** Template for the set of static methods on this runtime class. */
    //trait Statics
    
    /** Instance methods on this runtime class. */
    //val Methods: Methods
    /** Static methods on this runtime class. */
    //val Statics: Statics
  }
  
  class RuntimeAnnot(val Name: String) extends RuntimeClass {
    val Pkg = RuntimeAnnotPkg
  }
  
  object UnsignedAnnot extends RuntimeAnnot("Unsigned")
  object GoTypeAnnot   extends RuntimeAnnot("GoType")
  
  
  object Slice extends RuntimeClass {
    val Pkg  = RuntimePkg
    val Name = "Slice"
    
    private val ElemDesc = "Ljava/lang/Object;"
    
    object Methods {
      val Get = new AsmMethod("get", "(I)%s"  format ElemDesc)
      val Set = new AsmMethod("set", "(I%s)V" format ElemDesc)
      
      val Len = new AsmMethod("len", "()I")
      val Cap = new AsmMethod("cap", "()I")
      
      val SliceNone = new AsmMethod("slice",     "()%s"   format Desc)
      val SliceLow  = new AsmMethod("sliceLow",  "(I)%s"  format Desc)
      val SliceHigh = new AsmMethod("sliceHigh", "(I)%s"  format Desc)
      val SliceBoth = new AsmMethod("slice",     "(II)%s" format Desc)
    }
  }
  
  object Slices extends RuntimeClass {
    val Pkg  = RuntimePkg
    val Name = "Slices"
    
    object Methods {
      val MakeLen    = new AsmMethod("make", "(I)%s"  format Slice.Desc)
      val MakeLenCap = new AsmMethod("make", "(II)%s" format Slice.Desc)
      
      def fromArrayNone(t: RuntimeType) = new AsmMethod("fromArray",     "([%s)%s"   format (t.desc, Slice.Desc))
      def fromArrayLow (t: RuntimeType) = new AsmMethod("fromArrayLow",  "([%sI)%s"  format (t.desc, Slice.Desc))
      def fromArrayHigh(t: RuntimeType) = new AsmMethod("fromArrayHigh", "([%sI)%s"  format (t.desc, Slice.Desc))
      def fromArrayBoth(t: RuntimeType) = new AsmMethod("fromArray",     "([%sII)%s" format (t.desc, Slice.Desc))
    }
  }
}
