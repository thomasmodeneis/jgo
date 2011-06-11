package jgo.compiler
package interm
package instr

import symbol._
import types._
import member._

sealed abstract class Instr extends Product {
  override def toString = productIterator.mkString(productPrefix + " ", ", ", "")
  def listingString = toString
}

case object PrintString            extends Instr
case class  PrintNumeric(t: Arith) extends Instr

case class Decl(v: LocalVar)   extends Instr
case class Undecl(v: LocalVar) extends Instr

case class LoadVar(v:  Variable) extends Instr
case class StoreVar(v: Variable) extends Instr

sealed abstract class InvokeInstr extends Instr {
  val func: Func
}
case class InvokeFunction (func: Function) extends InvokeInstr
case class InvokeMethod   (func:   Method) extends InvokeInstr
case class InvokeInterface(func:   Method) extends InvokeInstr
case class InvokeLambda   (func:   Lambda) extends InvokeInstr

case class Go   (call: InvokeInstr) extends Instr
case class Defer(call: InvokeInstr) extends Instr

case class Function2Lambda(function: Function) extends Instr

case object Return      extends Instr
case object ValueReturn extends Instr

case class Wrap  (desiredType: WrappedType) extends Instr
case class Unwrap(currentType: WrappedType) extends Instr

case class Cast(oldType: Type, newType: Type) extends Instr
case class TypeAssert(t: Type)                extends Instr

case class  New(t: Type) extends Instr
case object Copy         extends Instr

case class MakeArray(t: Type, len: Int)             extends Instr
case class MakeSlice(t: Type, len: Int, cap: Int)   extends Instr
case class MakeMap(k: Type, v: Type)                extends Instr
case class MakeMapSize(k: Type, v: Type, size: Int) extends Instr
case class MakeChan(t: Type)                        extends Instr

case class  MkPtrVar(v: Variable) extends Instr
case class  MkPtrArray(t: Type)   extends Instr
case class  MkPtrSlice(t: Type)   extends Instr
case class  MkPtrField(f: Field)  extends Instr
case class  MkPtrPtr(t: Type)     extends Instr //this may be redundant; could I use just MkPtrObj?
case object MkPtrObj              extends Instr

case class PtrGet(t: Type) extends Instr
case class PtrPut(t: Type) extends Instr

case class GetField(f: Field, t: Type) extends Instr
case class PutField(f: Field, t: Type) extends Instr

case class  StrIndex(i: Integral)              extends Instr
case class  ArrayGet(i: Integral, elemT: Type) extends Instr
case class  SliceGet(i: Integral, elemT: Type) extends Instr
case object MapGet                             extends Instr

case class  ArrayPut(i: Integral, elemT: Type) extends Instr
case class  SlicePut(i: Integral, elemT: Type) extends Instr
case object MapPut                             extends Instr

case object ChanSend extends Instr
case object ChanRecv extends Instr

case class SliceArray(t: Type, bounds: SliceBounds) extends Instr
case class SliceSlice(t: Type, bounds: SliceBounds) extends Instr
case class Substring(bounds: SliceBounds)           extends Instr


case object StrAdd extends Instr

case class Neg(t: Arith) extends Instr

case class Add(t: Arith) extends Instr
case class Sub(t: Arith) extends Instr
case class Mul(t: Arith) extends Instr
case class Div(t: Arith) extends Instr

case class Mod(t: Integral) extends Instr

case class ShiftL(t1: Integral, t2: Unsigned) extends Instr
case class ShiftR(t1: Integral, t2: Unsigned) extends Instr

case class BitwiseAnd(t: Integral)    extends Instr
case class BitwiseOr(t: Integral)     extends Instr
case class BitwiseAndNot(t: Integral) extends Instr
case class BitwiseXor(t: Integral)    extends Instr

case class BitwiseCompl(t: Integral)    extends Instr

case class Incr(v: Variable, n: Int, t: Integral)  extends Instr
case class Decr(v: Variable, n: Int, t: Integral)  extends Instr


case class  PushStr (s: String)  extends Instr { override def toString = "PushStr \"" + s + "\"" }
case class  PushBool(b: Boolean) extends Instr
case object PushNil              extends Instr

case class PushInt    (i: BigInt,                    t: Integral) extends Instr
case class PushFloat  (f: BigDecimal,                t: Floating) extends Instr
case class PushComplex(r: BigDecimal, i: BigDecimal, t: Complex)  extends Instr


case object Pop       extends Instr
case object Dup       extends Instr
case object Dup_Down1 extends Instr
case object Dup_Down2 extends Instr
case object Swap      extends Instr

case class Duplicate(n: Int) extends Instr { require(n > 0) }


case class Lbl(l: Label) extends Instr { override def listingString = "--" + l + "--" }

sealed abstract class ControlFlow(name: String) extends Instr {
  val target: Label
  override def toString = name + ":  [" + target + "]"
}
case class Goto(target: Label)                        extends ControlFlow("goto")
case class Branch(test: BranchTest, target: Label)    extends ControlFlow("if " + test)
case class BranchNot(test: BranchTest, target: Label) extends ControlFlow("ifNot " + test)
