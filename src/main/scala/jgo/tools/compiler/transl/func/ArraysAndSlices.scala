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

trait ArraysAndSlices extends FuncTranslBase {
  protected override def translateInstr(i: Instr): Unit = i match {
    case ArrayGet(I32, et) => gen.arrayLoad(toAsmType(et))
    case ArrayPut(I32, et) => gen.arrayLoad(toAsmType(et))
    
    case SliceGet(I32, et) =>
      gen.invokeInterface(Slice.AsmType, Slice.Methods.Get)
      et.effective match {
        case pt: PrimitiveType =>
          //Contrary to the implication in the javadoc,
          //unbox takes Type.INT_TYPE, not Type.getObjectType("java/lang/Integer")
          //also causes a checkcast to be generated, so that's good.
          gen.unbox(toAsmType(pt))
        case _ =>
          gen.checkCast(toAsmType(et))
      }
    
    case SlicePut(I32, et) =>
      et.effective match {
        case pt: PrimitiveType => gen.box(toAsmType(pt))
        case _ => 
      }
      gen.invokeInterface(Slice.AsmType, Slice.Methods.Set)
    
    case SliceLen => gen.invokeInterface(Slice.AsmType, Slice.Methods.Len)
    case SliceCap => gen.invokeInterface(Slice.AsmType, Slice.Methods.Cap)
    
    case SliceSlice(it, bounds) => bounds match {
      case NoBounds   => gen.invokeInterface(Slice.AsmType, Slice.Methods.SliceNone)
      case LowBound   => gen.invokeInterface(Slice.AsmType, Slice.Methods.SliceLow)
      case HighBound  => gen.invokeInterface(Slice.AsmType, Slice.Methods.SliceHigh)
      case BothBounds => gen.invokeInterface(Slice.AsmType, Slice.Methods.SliceBoth)
    }
    
    case SliceArray(it, bounds) if it.effective == Int32 || it.effective == Uint32 => bounds match {
      case NoBounds =>
        gen.invokeStatic(IntSlice.AsmType, new AsmMethod("fromArray",     "([I)Ljgo/runtime/IntSlice;"))
      case LowBound =>
        gen.invokeStatic(IntSlice.AsmType, new AsmMethod("fromArrayLow",  "([II)Ljgo/runtime/IntSlice;"))
      case HighBound =>
        gen.invokeStatic(IntSlice.AsmType, new AsmMethod("fromArrayHigh", "([II)Ljgo/runtime/IntSlice;"))
      case BothBounds =>
        gen.invokeStatic(IntSlice.AsmType, new AsmMethod("fromArray",     "([III)Ljgo/runtime/IntSlice;"))
    }
    
    case SliceArray(t, bounds) => bounds match {
      case NoBounds =>
        gen.invokeStatic(IntSlice.AsmType, new AsmMethod("fromArray",     "([Ljava/lang/Object;)Ljgo/runtime/ObjSlice;"))
      case LowBound =>
        gen.invokeStatic(IntSlice.AsmType, new AsmMethod("fromArrayLow",  "([Ljava/lang/Object;I)Ljgo/runtime/ObjSlice;"))
      case HighBound =>
        gen.invokeStatic(IntSlice.AsmType, new AsmMethod("fromArrayHigh", "([Ljava/lang/Object;I)Ljgo/runtime/ObjSlice;"))
      case BothBounds =>
        gen.invokeStatic(IntSlice.AsmType, new AsmMethod("fromArray",     "([Ljava/lang/Object;II)Ljgo/runtime/ObjSlice;"))
    }
    
    case _ => super.translateInstr(i)
  }
}
