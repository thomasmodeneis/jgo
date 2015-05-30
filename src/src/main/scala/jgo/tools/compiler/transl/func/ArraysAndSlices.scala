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
  private def makeMultidimArray(t: ArrayType) = {
    var curT: Type = t
    var dim = 0
    while (curT.isInstanceOf[ArrayType]) {
      val at = curT.asInstanceOf[ArrayType]
      gen.push(at.length)
      dim += 1
      curT = at.elemType
    }
    gen.visitMultiANewArrayInsn(typeDesc(t), dim)
  }
  
  protected override def translateInstr(i: Instr): Unit = i match {
    case MakeArray(t) => t match {
      case ArrayType(_, _: ArrayType) => makeMultidimArray(t)
      case ArrayType(len, elemT)      => gen.push(len); gen.newArray(toAsmType(elemT))
    }
    
    case MakeSliceLen(_)    => gen.invokeStatic(Slices.AsmType, Slices.Methods.MakeLen)
    case MakeSliceLenCap(_) => gen.invokeStatic(Slices.AsmType, Slices.Methods.MakeLenCap)
    
    case ArrayGet(I32, et) => gen.arrayLoad(toAsmType(et))
    case ArrayPut(I32, et) => gen.arrayLoad(toAsmType(et))
    
    case SliceGet(I32, et) =>
      gen.invokeInterface(Slice.AsmType, Slice.Methods.Get)
      et.effective match {
        case pt: PrimitiveType =>
          //Contrary to the implication in the javadoc,
          //unbox takes Type.INT_TYPE, not Type.getObjectType("java/lang/Integer").
          ///Also, causes a checkcast to be generated, so that's good.
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
    
    case SliceArray(t, bounds) =>
      val runtimeT = toRuntimeStackType(t)
      bounds match {
        case NoBounds   => gen.invokeStatic(Slices.AsmType, Slices.Methods.fromArrayNone(runtimeT))
        case LowBound   => gen.invokeStatic(Slices.AsmType, Slices.Methods.fromArrayLow (runtimeT))
        case HighBound  => gen.invokeStatic(Slices.AsmType, Slices.Methods.fromArrayHigh(runtimeT))
        case BothBounds => gen.invokeStatic(Slices.AsmType, Slices.Methods.fromArrayBoth(runtimeT))
      }
    
    case _ => super.translateInstr(i)
  }
}
