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
 * 
 * The eval-underlying code of an lvalue is computed from its evaluation code
 * since, for example, a variable is not stored in its underlying form.
 */
private sealed abstract class LvalExpr extends UnderlyingFromEvalExpr {
  val typeOf: Type
  final def eval = load
  final def loadUnder = evalUnder
  
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
  override def mkPtr = EvalExpr(MkPtrVar(v), PointerType(typeOf))
}

private case class PtrDerefLval(ptr: Expr, typeOf: Type) extends LvalExpr {
  def load                        = ptr.evalUnder       |+| PtrGet(typeOf)
  def store(v: CodeBuilder)       = ptr.evalUnder |+| v |+| PtrPut(typeOf)
  def storePrefix(v: CodeBuilder) = ptr.evalUnder |+| v
  def storeSuffix                 =                         PtrPut(typeOf)
  
  override def addressable = true
  override def mkPtr = EvalExpr(ptr.eval |+| MkPtrPtr(typeOf), PointerType(typeOf))
}

private case class FieldLval(obj: Expr, f: Field) extends LvalExpr {
  val typeOf = f.typeOf
  
  def load                        = obj.evalUnder       |+| GetField(f, typeOf)
  def store(v: CodeBuilder)       = obj.evalUnder |+| v |+| PutField(f, typeOf)
  def storePrefix(v: CodeBuilder) = obj.evalUnder |+| v
  def storeSuffix                 =                         PutField(f, typeOf)
  
  override def addressable = obj.addressable
  override def mkPtr = EvalExpr(obj.eval |+| MkPtrField(f), PointerType(typeOf))
}

private case class ArrayIndexLval(array: Expr, index: Expr, typeOf: Type) extends LvalExpr {
  def load                        = array.evalUnder |+| index.evalUnder       |+| ArrayGet(typeOf)
  def store(v: CodeBuilder)       = array.evalUnder |+| index.evalUnder |+| v |+| ArrayPut(typeOf)
  def storePrefix(v: CodeBuilder) = array.evalUnder |+| index.evalUnder |+| v
  def storeSuffix                 =                                               ArrayPut(typeOf)
  
  override def addressable = array.addressable
  override def mkPtr = EvalExpr(array.eval |+| index.evalUnder |+| MkPtrArray(typeOf), PointerType(typeOf))
}

private case class SliceIndexLval(slice: Expr, index: Expr, typeOf: Type) extends LvalExpr {
  def load                        = slice.evalUnder |+| index.evalUnder       |+| SliceGet(typeOf)
  def store(v: CodeBuilder)       = slice.evalUnder |+| index.evalUnder |+| v |+| SlicePut(typeOf)
  def storePrefix(v: CodeBuilder) = slice.evalUnder |+| index.evalUnder |+| v
  def storeSuffix                 =                                               SlicePut(typeOf)
  
  override def addressable = true
  override def mkPtr = EvalExpr(slice.eval |+| index.eval |+| MkPtrSlice(typeOf), PointerType(typeOf))
}

private case class MapIndexLval(map: Expr, index: Expr, typeOf: Type) extends LvalExpr {
  //the ", ok" pattern is not currently supported
  def load                        = map.evalUnder |+| index.evalUnder       |+| MapGet
  def store(v: CodeBuilder)       = map.evalUnder |+| index.evalUnder |+| v |+| MapPut
  def storePrefix(v: CodeBuilder) = map.evalUnder |+| index.evalUnder |+| v
  def storeSuffix                 =                                             MapPut
}
