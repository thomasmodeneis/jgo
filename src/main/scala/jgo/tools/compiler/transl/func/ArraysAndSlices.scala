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

trait ArraysAndSlices extends FuncTranslBase {
  protected override def translateInstr(i: Instr): Unit = i match {
    case ArrayGet(I32, et) => gen.arrayLoad(toAsmType(et))
    case ArrayPut(I32, et) => gen.arrayLoad(toAsmType(et))
    
    case SliceGet(I32, et) =>
      gen.invokeInterface(SliceAsmType, new AsmMethod("get", "(I)Ljava/lang/object;"))
      gen.checkCast(toAsmType(et))
    case SlicePut(I32, et) =>
      gen.invokeInterface(SliceAsmType, new AsmMethod("set", "(ILjava/lang/object;)V"))
    
    case SliceSlice(it, bounds) => bounds match {
      case NoBounds =>
        gen.invokeInterface(SliceAsmType, new AsmMethod("slice",     "()Ljgo/runtime/Slice;"))
      case LowBound =>
        gen.invokeInterface(SliceAsmType, new AsmMethod("sliceLow",  "(I)Ljgo/runtime/Slice;"))
      case HighBound =>
        gen.invokeInterface(SliceAsmType, new AsmMethod("sliceHigh", "(I)Ljgo/runtime/Slice;"))
      case BothBounds =>
        gen.invokeInterface(SliceAsmType, new AsmMethod("slice",     "(II)Ljgo/runtime/Slice;"))
    }
    
    case SliceArray(it, bounds) if it.effective == Int32 || it.effective == Uint32 => bounds match {
      case NoBounds =>
        gen.invokeStatic(IntSliceAsmType, new AsmMethod("fromArray",     "([I)Ljgo/runtime/IntSlice;"))
      case LowBound =>
        gen.invokeStatic(IntSliceAsmType, new AsmMethod("fromArrayLow",  "([II)Ljgo/runtime/IntSlice;"))
      case HighBound =>
        gen.invokeStatic(IntSliceAsmType, new AsmMethod("fromArrayHigh", "([II)Ljgo/runtime/IntSlice;"))
      case BothBounds =>
        gen.invokeStatic(IntSliceAsmType, new AsmMethod("fromArray",     "([III)Ljgo/runtime/IntSlice;"))
    }
    
    case SliceArray(t, bounds) => bounds match {
      case NoBounds =>
        gen.invokeStatic(IntSliceAsmType, new AsmMethod("fromArray",     "([Ljava/lang/Object;)Ljgo/runtime/ObjSlice;"))
      case LowBound =>
        gen.invokeStatic(IntSliceAsmType, new AsmMethod("fromArrayLow",  "([Ljava/lang/Object;I)Ljgo/runtime/ObjSlice;"))
      case HighBound =>
        gen.invokeStatic(IntSliceAsmType, new AsmMethod("fromArrayHigh", "([Ljava/lang/Object;I)Ljgo/runtime/ObjSlice;"))
      case BothBounds =>
        gen.invokeStatic(IntSliceAsmType, new AsmMethod("fromArray",     "([Ljava/lang/Object;II)Ljgo/runtime/ObjSlice;"))
    }
    
    case _ => super.translateInstr(i)
  }
}
