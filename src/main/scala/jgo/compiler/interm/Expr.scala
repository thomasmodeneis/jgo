package jgo.compiler
package interm

import instr._
import types._
import symbols._
import codeseq._
import member._
import bool._

sealed abstract class Expr extends Typed {
  /**
   * Provides the code necessary for computing the value
   * of this expression and placing the result on the top
   * of the operand stack. This code is called the
   * <i>evaluation code</i> of this expression.
   */
  def eval: CodeBuilder
  def call(args: List[Expr]): CodeBuilder = {
    require(callable)
    (args foldLeft eval) { _ |+| _.eval }
  }
  val addressable = false
}

case object ExprError extends LvalExpr {
  val typeOf                = TypeError
  def load                  = CodeBuilder.empty
  def store(v: CodeBuilder) = CodeBuilder.empty
  override val addressable  = true
}

case class SimpleExpr(eval: CodeBuilder, typeOf: Type) extends Expr

case class FuncExpr(f: Function) extends Expr {
  val typeOf = f.typeOf
  override val callable = true
  def eval = Func2Lambda(f)
  override def call(args: List[Expr]): CodeBuilder =
    (args foldLeft CodeBuilder.empty) { _ |+| _.eval } |+| InvokeFunc(f)
}

case class BoolExpr(tree: BoolTree) extends Expr {
  val typeOf            = Bool
  override val callable = false
  def eval = tree.evalAsBool
  
  def branchTo(lbl: Label)                           = tree.branchTo(lbl)
  def mkIf(ifB: CodeBuilder)                         = tree.mkIf(ifB)
  def mkIfElse(ifB: CodeBuilder, elseB: CodeBuilder) = tree.mkIfElse(ifB, elseB)
  def mkWhile(loopBody: CodeBuilder)                 = tree.mkWhile(loopBody)
}

//case class ConstExpr(value: Any, typeOf: ConstType) extends Expr(typeOf)

sealed abstract class LvalExpr extends Expr {
  val typeOf: Type
  def load: CodeBuilder
  def store(value: CodeBuilder): CodeBuilder
  final def eval = load
}
case class VarLval(v: Variable) extends LvalExpr {
  val typeOf                 = v.typeOf
  def load                   =         LoadVar(v)
  def store(vl: CodeBuilder) = vl |+| StoreVar(v)
  override val addressable   = true
}
case class PtrLval(ptr: Expr) extends LvalExpr {
  val typeOf = ptr.t.underlying.asInstanceOf[PointerType].elemType
  def load                  = ptr.eval       |+| Deref  //this needs to be fleshed out, so to speak, substantially
  def store(v: CodeBuilder) = ptr.eval |+| v |+| PutRef //yeah; codegen is going to need more info than this
  override val addressable = true
}
case class FieldLval(obj: Expr, f: Field) extends LvalExpr {
  val typeOf                = f.t
  def load                  = obj.eval       |+| GetField(f, this.typeOf)
  def store(v: CodeBuilder) = obj.eval |+| v |+| PutField(f, this.typeOf)
  override val addressable  = obj.addressable
}
case class ArrayIndexLval(array: Expr, index: Expr) extends LvalExpr {
  val typeOf                = array.t.underlying.asInstanceOf[ArrayType].elemType
  def load                  = array.eval |+| index.eval       |+| ArrayGet(this.typeOf)
  def store(v: CodeBuilder) = array.eval |+| index.eval |+| v |+| ArrayPut(this.typeOf)
  override val addressable  = array.addressable
}
case class SliceIndexLval(slice: Expr, index: Expr) extends LvalExpr {
  val typeOf                = slice.t.underlying.asInstanceOf[SliceType].elemType
  def load                  = slice.eval |+| index.eval       |+| SliceGet(this.typeOf)
  def store(v: CodeBuilder) = slice.eval |+| index.eval |+| v |+| SlicePut(this.typeOf)
  override val addressable  = true
}
case class MapIndexLval(map: Expr, index: Expr) extends LvalExpr {
  val typeOf = map.t.underlying.asInstanceOf[MapType].keyType
  //the ", ok" pattern is not currently supported
  def load                  = map.eval |+| index.eval       |+| MapGet
  def store(v: CodeBuilder) = map.eval |+| index.eval |+| v |+| MapPut
}
