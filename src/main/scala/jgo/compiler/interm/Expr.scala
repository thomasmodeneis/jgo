package jgo.compiler
package interm

import instr._
import types._
import symbols._
import codeseq._
import member._

sealed abstract class Expr {
  val typeOf: Type
  /**
   * Provides the code necessary for computing the value
   * of this expression and placing the result on the top
   * of the operand stack. This code is called the
   * <i>evaluation code</i> of this expression.
   */
  def eval: CodeBuilder
}

case class SimpleExpr(eval: CodeBuilder, typeOf: Type) extends Expr
//case class BoolExpr(tree: BoolTree) extends Expr(Bool)
//case class ConstExpr(value: Any, typeOf: ConstType) extends Expr(typeOf)

sealed abstract class LvalExpr(val typeOf: Type) extends Expr {
  final def eval = load
  def load: CodeBuilder
  def store(value: CodeBuilder): CodeBuilder
}
case class VarLval(v: Variable) extends LvalExpr(v.typeOf) {
  def load                  =         LoadVar(v)
  def store(vl: CodeBuilder) = vl |+| StoreVar(v)
}
case class FieldLval(obj: Expr, f: Field) extends LvalExpr(f.typeOf) {
  def load                  = obj.eval       |+| GetField(f, this.typeOf)
  def store(v: CodeBuilder) = obj.eval |+| v |+| PutField(f, this.typeOf)
}
case class ArrayIndexLval(array: Expr, index: Expr) extends LvalExpr(array.typeOf.underlying.asInstanceOf[ArrayType].elemType) {
  def load                  = array.eval |+| index.eval       |+| ArrayGet(this.typeOf)
  def store(v: CodeBuilder) = array.eval |+| index.eval |+| v |+| ArrayPut(this.typeOf)
}
case class SliceIndexLval(slice: Expr, index: Expr) extends LvalExpr(slice.typeOf.underlying.asInstanceOf[SliceType].elemType) {
  def load                  = slice.eval |+| index.eval       |+| SliceGet(this.typeOf)
  def store(v: CodeBuilder) = slice.eval |+| index.eval |+| v |+| SlicePut(this.typeOf)
}
case class MapIndexLval(map: Expr, index: Expr) extends LvalExpr(map.typeOf.underlying.asInstanceOf[MapType].keyType) {
  //the ", ok" pattern is not currently supported
  def load                  = map.eval |+| index.eval       |+| MapGet
  def store(v: CodeBuilder) = map.eval |+| index.eval |+| v |+| MapPut
}
