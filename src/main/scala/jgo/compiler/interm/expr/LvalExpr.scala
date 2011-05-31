package jgo.compiler
package interm
package expr

import member._

import types._
import symbol._
import instr._
import instr.TypeConversions._
import codeseq._

/**
 * An expression that can be assigned to.  Such expressions are called ''lvalues''.
 * In addition to having evaluation code -- i.e., code that ''loads'' the current
 * value of an lvalue -- lvalues also have methods for producing ''storage code''
 * -- code that sets an lvalue to a certain value, given code for placing that value
 * atop the operand stack.  Finally, lvalues support generation of ''split storage
 * code'', a feature used in the implementation of multiple assignment.
 */
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
  override def mkPtr = BasicExpr(MkPtrVar(v), PointerType(typeOf))
}

private case class PtrDerefLval(ptr: Expr, typeOf: Type) extends LvalExpr {
  def load                        = ptr.eval       |+| PtrGet(typeOf)
  def store(v: CodeBuilder)       = ptr.eval |+| v |+| PtrPut(typeOf)
  def storePrefix(v: CodeBuilder) = ptr.eval |+| v
  def storeSuffix                 =                    PtrPut(typeOf)
  
  override def addressable = true
  override def mkPtr = BasicExpr(ptr.eval |+| MkPtrPtr(typeOf), PointerType(typeOf))
}

private case class FieldLval(obj: Expr, f: Field) extends LvalExpr {
  val typeOf = f.typeOf
  
  def load                        = obj.eval       |+| GetField(f, typeOf)
  def store(v: CodeBuilder)       = obj.eval |+| v |+| PutField(f, typeOf)
  def storePrefix(v: CodeBuilder) = obj.eval |+| v
  def storeSuffix                 =                    PutField(f, typeOf)
  
  override def addressable = obj.addressable
  override def mkPtr = BasicExpr(obj.eval |+| MkPtrField(f), PointerType(typeOf))
}

private case class ArrayIndexLval(array: Expr, index: Expr, typeOf: Type) extends LvalExpr {
  def load                        = array.eval |+| index.eval       |+| ArrayGet(typeOf)
  def store(v: CodeBuilder)       = array.eval |+| index.eval |+| v |+| ArrayPut(typeOf)
  def storePrefix(v: CodeBuilder) = array.eval |+| index.eval |+| v
  def storeSuffix                 =                                     ArrayPut(typeOf)
  
  override def addressable = array.addressable
  override def mkPtr = BasicExpr(array.eval |+| index.eval |+| MkPtrArray(typeOf), PointerType(typeOf))
}

private case class SliceIndexLval(slice: Expr, index: Expr, typeOf: Type) extends LvalExpr {
  def load                        = slice.eval |+| index.eval       |+| SliceGet(typeOf)
  def store(v: CodeBuilder)       = slice.eval |+| index.eval |+| v |+| SlicePut(typeOf)
  def storePrefix(v: CodeBuilder) = slice.eval |+| index.eval |+| v
  def storeSuffix                 =                                     SlicePut(typeOf)
  
  override def addressable = true
  override def mkPtr = BasicExpr(slice.eval |+| index.eval |+| MkPtrSlice(typeOf), PointerType(typeOf))
}

private case class MapIndexLval(map: Expr, index: Expr, typeOf: Type) extends LvalExpr {
  //the ", ok" pattern is not currently supported
  def load                        = map.eval |+| index.eval       |+| MapGet
  def store(v: CodeBuilder)       = map.eval |+| index.eval |+| v |+| MapPut
  def storePrefix(v: CodeBuilder) = map.eval |+| index.eval |+| v
  def storeSuffix                 =                                   MapPut
}
