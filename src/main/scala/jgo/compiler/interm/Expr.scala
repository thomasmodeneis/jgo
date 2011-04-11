package jgo.compiler
package interm

import instr._
import types._
import codeseq._

sealed abstract class Expr(val typeOf: Type)

case class ExprCode(code: CodeBuilder, typeOf: Type) extends Expr(typeOf)
case class BoolExpr(tree: BoolTree) extends Expr(Bool)
case class ConstExpr(value: Any, typeOf: ConstType) extends Expr(typeOf)

sealed abstract class LvalExpr(val typeOf: Type) extends Expr(typeOf) {
  def load: CodeBuilder
  def store(value: CodeBuilder): CodeBuilder
}
case class VarLval(v: Variable) extends LvalExpr(v.typeOf) {
  def load = CodeBuilder(LoadVar(v))
  def store(v: CodeBuilder) = v += StoreVar(v)
}
case class ArrayIndexLval(array: LvalExpr, index: Expr) extends LvalExpr(arr.typeOf.underlying.asInstanceOf[ArrayType].elemType) {
  def load = (array |+| index) += ArrayGet(this.typeOf)
  def store(v: CodeBuilder) = (array |+| index |+| v) += ArrayPut(this.typeOf)
}
case class SliceIndexLval(slice: LvalExpr, index: Expr) extends LvalExpr(arr.typeOf.underlying.asInstanceOf[SliceType].elemType) {
  def load = (slice |+| index) += SliceGet(this.typeOf)
  def store(v: CodeBuilder) = (slice |+| index |+| v) += SlicePut(this.typeOf)
}
//case class MapIndexLval(map: LvalExpr, index: Expr) extends 
