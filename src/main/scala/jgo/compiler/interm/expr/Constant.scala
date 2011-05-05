package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

import PartialFunction._

sealed abstract class Constant extends Expr {
  
}

private sealed abstract class NumericConst {
  val value: BigDecimal
}

private case class UntypedNumericConst(value: BigDecimal) extends Constant {
  def eval =
    throw new UnsupportedOperationException("Impl error: attempting to call eval on an UntypedConst")
  
  def isBounded(low: BigDecimal, high: BigDecimal) =
    (low <= value) && (value <= high)
    
  def isInt =
    value % 1 == 0
    
  val typeOf = new UntypedConstType {
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
      
      case Complex64  => throw new UnsupportedOperationException("complex constants not yet supported") //value.floatValue.isInfinite
      case Complex128 => throw new UnsupportedOperationException("complex constants not yet supported") //value.doubleValue.isInfinite
    }
    
    val semantics = Primitive
  }
    
  /*
  def withType(newType: NumericType): M[Constant] = newType match {
    
  }
  */
}
