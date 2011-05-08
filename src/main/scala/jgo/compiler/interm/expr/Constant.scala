package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

import PartialFunction._

sealed abstract class Constant extends Expr

private case class StringConst(value: String) extends Constant {
  val typeOf = StringType
  def eval   = PushStr(value)
}

private case class BoolConst(value: Boolean) extends Constant {
  val typeOf = BoolType
  def eval   = PushBool(value)
}

private case class IntConst(value: Int) extends Constant {
  val typeOf = Int32
  def eval   = PushInt(value, typeOf)
}

private case class FloatConst(value: Double) extends Constant {
  val typeOf = Float64
  def eval   = PushFloat(value, typeOf)
}

/*
private sealed abstract class RealConst extends Constant {
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
