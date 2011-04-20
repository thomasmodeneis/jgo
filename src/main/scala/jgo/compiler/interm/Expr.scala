package jgo.compiler
package interm

import instr._
import types._
import symbols._
import codeseq._
import member._
import bool._

object Expr {
  private def ordinal(n: Int): String = {
    require(n >= 0)
    n match {
      case 0 => "zeroth"
      case 1 => "first"
      case 2 => "second"
      case 3 => "third"
      case 4 => "fourth"
      case 5 => "fifth"
      case 6 => "sixth"
      case 7 => "seventh"
      case 8 => "eighth"
      case 9 => "ninth"
      case i => i.toString + "th"
    }
  }
}

sealed abstract class Expr extends Typed {
  /**
   * Provides the code necessary for computing the value
   * of this expression and placing the result on the top
   * of the operand stack. This code is called the
   * <i>evaluation code</i> of this expression.
   */
  def eval: CodeBuilder
  
  val addressable = false
  
  final def checkCall(args: List[Expr]): Either[String, Type] = this.funcType match {
    case None => Left("not of function type")
    
    case Some(FuncType(_, List(res0, res1, _*), _)) =>
      Left("polyadic results not currently supported")
    
    case Some(FuncType(params, results, false)) =>
      if (params.length != args.length)
        Left("number (%d) of arguments passed unequal to number (%d) required" format (args.length, params.length))
      else {
        for (((param, HasType(arg)), index) <- (params zip args).zipWithIndex)
          if (!(param <<= arg))
            return Left("%s argument has type %s not assignable to corresponding parameter type %s" format (index, arg, param))
        Right(results.headOption getOrElse UnitType)
      }
    
    case Some(FuncType(params, results, true)) =>
      Left("variadic calls not yet supported")
  }
    
  def call(args: List[Expr]): Either[String, Expr] =
    for (resultT <- checkCall(args).right)
    yield SimpleExpr((args foldLeft eval) { _ |+| _.eval } |+| InvokeLambda(funcType.get), resultT)
}

case object ExprError extends LvalExpr {
  val typeOf                          = TypeError
  def load                            = CodeBuilder()
  def store(v: CodeBuilder)           = CodeBuilder()
  def storePrefix(value: CodeBuilder) = CodeBuilder()
  def storeSuffix: CodeBuilder        = CodeBuilder()
  override val addressable  = true
}

case class SimpleExpr(eval: CodeBuilder, typeOf: Type) extends Expr

case class FuncExpr(f: Function) extends Expr {
  val typeOf = f.typeOf
  override val callable = true
  
  def eval = Func2Lambda(f)
  override def call(args: List[Expr]): Either[String, Expr] =
    for (resultT <- checkCall(args).right)
    yield SimpleExpr((args foldLeft CodeBuilder()) { _ |+| _.eval } |+| InvokeFunc(f), resultT)
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
  final def eval = load
  
  def load: CodeBuilder
  def store(value: CodeBuilder): CodeBuilder
  def storePrefix(value: CodeBuilder): CodeBuilder
  def storeSuffix: CodeBuilder
}

case class VarLval(v: Variable) extends LvalExpr {
  val typeOf                 = v.typeOf
  
  def load                   =         LoadVar(v)
  def store(vl: CodeBuilder) = vl |+| StoreVar(v)
  def storePrefix(vl: CodeBuilder) = CodeBuilder.empty |+| vl
  def storeSuffix                  = StoreVar(v)
  
  override val addressable   = true
}

case class PtrLval(ptr: Expr) extends LvalExpr {
  val typeOf = ptr.t.underlying.asInstanceOf[PointerType].elemType
  
  def load                        = ptr.eval       |+| Deref  //this needs to be fleshed out, so to speak, substantially
  def store(v: CodeBuilder)       = ptr.eval |+| v |+| PutRef //yeah; codegen is going to need more info than this
  def storePrefix(v: CodeBuilder) = ptr.eval |+| v
  def storeSuffix                 =                    PutRef
  
  override val addressable = true
}

case class FieldLval(obj: Expr, f: Field) extends LvalExpr {
  val typeOf = f.t
  
  def load                        = obj.eval       |+| GetField(f, this.typeOf)
  def store(v: CodeBuilder)       = obj.eval |+| v |+| PutField(f, this.typeOf)
  def storePrefix(v: CodeBuilder) = obj.eval |+| v
  def storeSuffix                 =                    PutField(f, this.typeOf)
  
  override val addressable = obj.addressable
}

case class ArrayIndexLval(array: Expr, index: Expr) extends LvalExpr {
  val typeOf = array.t.underlying.asInstanceOf[ArrayType].elemType
  
  def load                        = array.eval |+| index.eval       |+| ArrayGet(this.typeOf)
  def store(v: CodeBuilder)       = array.eval |+| index.eval |+| v |+| ArrayPut(this.typeOf)
  def storePrefix(v: CodeBuilder) = array.eval |+| index.eval |+| v
  def storeSuffix                 =                                     ArrayPut(this.typeOf)
  
  override val addressable = array.addressable
}

case class SliceIndexLval(slice: Expr, index: Expr) extends LvalExpr {
  val typeOf = slice.t.underlying.asInstanceOf[SliceType].elemType
  
  def load                        = slice.eval |+| index.eval       |+| SliceGet(this.typeOf)
  def store(v: CodeBuilder)       = slice.eval |+| index.eval |+| v |+| SlicePut(this.typeOf)
  def storePrefix(v: CodeBuilder) = slice.eval |+| index.eval |+| v
  def storeSuffix                 = SlicePut(this.typeOf)
  
  override val addressable = true
}

case class MapIndexLval(map: Expr, index: Expr) extends LvalExpr {
  val typeOf = map.t.underlying.asInstanceOf[MapType].keyType
  
  //the ", ok" pattern is not currently supported
  def load                        = map.eval |+| index.eval       |+| MapGet
  def store(v: CodeBuilder)       = map.eval |+| index.eval |+| v |+| MapPut
  def storePrefix(v: CodeBuilder) = map.eval |+| index.eval |+| v
  def storeSuffix                 =                                   MapPut
}
