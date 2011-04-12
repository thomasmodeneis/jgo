package jgo.compiler
package interm
package instr

import symbols._
import types._
import member._

sealed abstract class Instr

case class Decl(v: LocalVar) extends Instr
case class UnDecl(v: LocalVar) extends Instr

case object Deref extends Instr
case object Enref extends Instr
case object Copy  extends Instr

case class New(typeOf: Type) extends Instr

case class  InvokeFunc(func: Function) extends Instr
case class  InvokeLambda(t: FuncType) extends Instr
case class  Func2Lambda(func: Function) extends Instr
case object Return extends Instr

case class Add(t: Arith) extends Instr
case class Sub(t: Arith) extends Instr
case class Mul(t: Arith) extends Instr
case class Div(t: Arith) extends Instr

case class Mod(t: Integral) extends Instr

case class ShiftL(t1: Integral, t2: Unsigned) extends Instr
case class ShiftR(t1: Integral, t2: Unsigned) extends Instr

case class Goto(target: Label) extends Instr
//case class Branch(b: BooleanTree, target: Label) extends Instr
case class Lbl(l: Label) extends Instr

case object Pop       extends Instr
case object Dup       extends Instr
case object Dup_Down1 extends Instr
case object Dup_Down2 extends Instr
case object Swap      extends Instr

case class LoadVar(v:  Variable) extends Instr
case class StoreVar(v: Variable) extends Instr

case class GetField(f: Field, t: Type) extends Instr
case class PutField(f: Field, t: Type) extends Instr

case class ArrayGet(elemT: Type) extends Instr
case class SliceGet(elemT: Type) extends Instr
case object MapGet extends Instr

case class ArrayPut(elemT: Type) extends Instr
case class SlicePut(elemT: Type) extends Instr
case object MapPut extends Instr

case class ChanSend(elemT: Type) extends Instr
case class ChanRecv(elemT: Type) extends Instr
