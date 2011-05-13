package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

import PartialFunction._

sealed abstract class ConstExpr extends Expr

case class StringConst(value: String) extends ConstExpr {
  val typeOf = StringType
  def eval   = PushStr(value)
}

/*object BoolConst {
  val True  = new BoolConst(true)
  val False = new BoolConst(false)
  
  @inline
  def apply(bool: Boolean): BoolConst =
    if (bool) True
    else False
}*/

case class BoolConst(value: Boolean) extends ConstExpr {
  val typeOf = BoolType
  def eval   = PushBool(value)
}

private case class IntConst(value: Int) extends ConstExpr {
  val typeOf = Int32
  def eval   = PushInt(value, typeOf)
}

private case class FloatConst(value: Double) extends ConstExpr {
  val typeOf = Float64
  def eval   = PushFloat(value, typeOf)
}

object NilConst extends ConstExpr {
  val typeOf = NilType
  def eval   = PushNil
}

/*
private sealed abstract class RealConst extends ConstExpr {
  val value: BigDecimal
}

private case class UntypedRealConst(value: BigDecimal) extends RealConst {
  def eval =
    throw new UnsupportedOperationException("Impl error: attempting to call eval on an UntypedConst")
  
  private def isBounded(low: BigDecimal, high: BigDecimal) =
    (low <= value) && (value <= high)
    
  def isInt =
    value % 1 == 0
  
  def canFitIn(t: BuiltinType) = cond(t) {
    case Int8  => value.isValidByte
    case Int16 => value.isValidShort
    case Int32 => value.isValidInt
    case Int64 => isInt && isBounded(Long.MinValue, Long.MaxValue)
    
    case Uint8  => isInt && isBounded(0, 255)
    case Uint16 => isInt && isBounded(0, (1 << 16) - 1)
    case Uint32 => isInt && isBounded(0, (1 << 32) - 1)
    case Uint64 => isInt && isBounded(0, (1 << 64) - 1)
    
    case Float32 => value.floatValue.isInfinite //Float.MinValue <= value && value <=  Float.MaxValue
    case Float64 => value.doubleValue.isInfinite //Double.MinValue <= value && value <= Double.MaxValue
    
    case Complex64  => false //value.floatValue.isInfinite
    case Complex128 => false //value.doubleValue.isInfinite
  }
  
  val typeOf = new UntypedNumericConstType {
    override def toString = "<const " + value + ">"
    def canFitIn(t: BuiltinType) = UntypedRealConst.this.canFitIn(t)
    val semantics = Primitive
  }
}

private case class TypedRealConst(value: BigDecimal, typeOf: RealType) extends RealConst {
  def eval = typeOf match {
    case ft: FloatingType => PushFloat(value.doubleValue, ft)
    case it: IntegralType => PushInt(value.longValue, it)
  }
}
*/
