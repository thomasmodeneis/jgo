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
  
  val RuntimePkg:         PkgLoc = "jgo/runtime/"
  val RuntimeInternalPkg: PkgLoc = "jgo/runtime/internal/"
  
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
    val Pkg = RuntimeInternalPkg
  }
  
  object UnsignedAnnot extends RuntimeAnnot("Unsigned")
  object GoTypeAnnot   extends RuntimeAnnot("GoType")
  
  
  class SliceLike(val Name: String, val ElemName: String) extends RuntimeClass {
    val Pkg = RuntimePkg
    
    val ElemDesc = "Ljava/lang/%s;" format ElemName
    
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
  
  trait SliceImplLike extends RuntimeClass {
    object Statics {
      val FromArrayNone = new AsmMethod("fromArray",     "([I)%s"   format Desc)
      val FromArrayLow  = new AsmMethod("fromArrayLow",  "([II)%s"  format Desc)
      val FromArrayHigh = new AsmMethod("fromArrayHigh", "([II)%s"  format Desc)
      val FromArrayBoth = new AsmMethod("fromArray",     "([III)%s" format Desc)
    }
  }
  
  object Slice    extends SliceLike("Slice",    "Object")
  object IntSlice extends SliceLike("IntSlice", "Integer") with SliceImplLike
  object ObjSlice extends SliceLike("ObjSlice", "Object")  with SliceImplLike
}
