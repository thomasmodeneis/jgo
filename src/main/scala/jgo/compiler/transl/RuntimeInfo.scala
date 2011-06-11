package jgo.compiler
package transl

import interm._
import codeseq._
import instr._
import symbol._
import types._

import org.objectweb.asm.{Type => AsmType}

object RuntimeInfo {
  val RuntimeLocation = "jgo/runtime/"
  
  val UnsignedAnnot = RuntimeLocation + "Unsigned"
  val GoTypeAnnot = RuntimeLocation + "GoType"
  
  val SliceClass    = RuntimeLocation + "Slice"
  val SliceDesc     = "L%s%s;" format (RuntimeLocation, "Slice")
  val SliceAsmType  = AsmType.getObjectType(SliceClass)
  
  val IntSliceClass    = RuntimeLocation + "IntSlice"
  val IntSliceDesc     = "L%s%s;" format (RuntimeLocation, "IntSlice")
  val IntSliceAsmType  = AsmType.getObjectType(SliceClass)
  
  val ObjSliceClass    = RuntimeLocation + "ObjSlice"
  val ObjSliceDesc     = "L%s%s;" format (RuntimeLocation, "ObjSlice")
  val ObjSliceAsmType  = AsmType.getObjectType(SliceClass)
}
