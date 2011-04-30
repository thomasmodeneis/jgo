package jgo.compiler
package interm
package expr

import member._

import types._
import symbol._
import instr._
import instr.TypeConversions._
import codeseq._

private sealed abstract class LvalExpr extends Expr {
  val typeOf: Type
  final def eval = load
  
  def load: CodeBuilder
  def store(value: CodeBuilder): CodeBuilder
  def storePrefix(value: CodeBuilder): CodeBuilder
  def storeSuffix: CodeBuilder
}

private case class VarLval(v: Variable) extends LvalExpr {
  val typeOf = v.typeOf
  
  def load                         =         LoadVar(v)
  def store(vl: CodeBuilder)       = vl |+| StoreVar(v)
  def storePrefix(vl: CodeBuilder) = vl
  def storeSuffix                  =        StoreVar(v)
  
  override def addressable = true
}

private case class PtrLval(ptr: Expr) extends LvalExpr {
  val typeOf = ptr.t.underlying.asInstanceOf[PointerType].elemType
  
  def load                        = ptr.eval       |+| Deref  //this needs to be fleshed out, so to speak, substantially
  def store(v: CodeBuilder)       = ptr.eval |+| v |+| PutRef //yeah; codegen is going to need more info than this
  def storePrefix(v: CodeBuilder) = ptr.eval |+| v
  def storeSuffix                 =                    PutRef
  
  override def addressable = true
}

private case class FieldLval(obj: Expr, f: Field) extends LvalExpr {
  val typeOf = f.t
  
  def load                        = obj.eval       |+| GetField(f, this.typeOf)
  def store(v: CodeBuilder)       = obj.eval |+| v |+| PutField(f, this.typeOf)
  def storePrefix(v: CodeBuilder) = obj.eval |+| v
  def storeSuffix                 =                    PutField(f, this.typeOf)
  
  override def addressable = obj.addressable
}

private case class ArrayIndexLval(array: Expr, index: Expr, typeOf: Type) extends LvalExpr {
  //val typeOf = array.t.underlying.asInstanceOf[ArrayType].elemType
  
  def load                        = array.eval |+| index.eval       |+| ArrayGet(this.typeOf)
  def store(v: CodeBuilder)       = array.eval |+| index.eval |+| v |+| ArrayPut(this.typeOf)
  def storePrefix(v: CodeBuilder) = array.eval |+| index.eval |+| v
  def storeSuffix                 =                                     ArrayPut(this.typeOf)
  
  override def addressable = array.addressable
}

private case class SliceIndexLval(slice: Expr, index: Expr, typeOf: Type) extends LvalExpr {
  //val typeOf = slice.t.underlying.asInstanceOf[SliceType].elemType
  
  def load                        = slice.eval |+| index.eval       |+| SliceGet(this.typeOf)
  def store(v: CodeBuilder)       = slice.eval |+| index.eval |+| v |+| SlicePut(this.typeOf)
  def storePrefix(v: CodeBuilder) = slice.eval |+| index.eval |+| v
  def storeSuffix                 = SlicePut(this.typeOf)
  
  override def addressable = true
}

private case class MapIndexLval(map: Expr, index: Expr, typeOf: Type) extends LvalExpr {
  //val typeOf = map.t.underlying.asInstanceOf[MapType].keyType
  
  //the ", ok" pattern is not currently supported
  def load                        = map.eval |+| index.eval       |+| MapGet
  def store(v: CodeBuilder)       = map.eval |+| index.eval |+| v |+| MapPut
  def storePrefix(v: CodeBuilder) = map.eval |+| index.eval |+| v
  def storeSuffix                 =                                   MapPut
}
