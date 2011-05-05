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

private case class UntypedNumericConst(value: BigDecimal) extends Constant {
  def eval =
    throw new UnsupportedOperationException("Impl error: attempting to call eval on an UntypedConst")
  
  val typeOf = new UntypedConstType {
    private def bounded(low: BigDecimal, high: BigDecimal) =
      (low <= value) && (value <= high)
    
    private def 
    
    def canFitIn(t: BuiltinType) = cond(t) {
      case Int8  => value.isValidByte
      case Int16 => value.isValidShort
      case Int32 => value.isValidInt
      case Int64 => bounded(Long.MinValue, Long.MaxValue)
      
      case Uint8  => bounded(0, 255)
      case Uint16 => bounded(0, (1 << 16) - 1)
      case Uint32 => bounded(0, (1 << 32) - 1)
      case Uint64 => bounded(0, (1 << 64) - 1)
      
      case Float32 => value.floatValue.isInfinite //Float.MinValue <= value && value <=  Float.MaxValue
      case Float64 => value.doubleValue.isInfinite //Double.MinValue <= value && value <= Double.MaxValue
      
      case Complex64  => value.floatValue.isInfinite
      case Complex128 => value.doubleValue.isInfinite
      
    }
    
    /*
    def withType(newType: NumericType): M[Constant] = newType match {
      
    }
    */
    
    val semantics = Primitive
  }
}

private sealed abstract class TypedConst {
  val typeOf: ConstableType
}

private case class IntegralConst(value: Long, typeOf: IntegralType) extends Constant {
  def eval = IntConst(value, typeOf)
  val semantics  = Primitive
}

private case class FloatingPtConst(value: Double
