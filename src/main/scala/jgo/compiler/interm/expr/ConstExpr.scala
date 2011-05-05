package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

import PartialFunction._

private sealed abstract class ConstExpr extends Expr {
}

private sealed abstract class UntypedConst extends ConstExpr {
  val typeOf: UntypedConstType
  def eval =
    throw new UnsupportedOperationException("Impl error: attempting to call eval on an UntypedConst")
}

private case class UntypedIntConst(value: BigInt) extends UntypedConst {
  val typeOf = new UntypedConstType {
    private def bounded(low: BigInt, high: BigInt) =
      (low <= value) && (value <= high)
    
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
      case Float64 => value.floatValue.isInfinite //Double.MinValue <= value && value <= Double.MaxValue
    }
    
    val semantics = Primitive
  }
}

private case class IntConstExpr(value: BigInt, typeOf: IntegralType) extends ConstExpr {
  def eval = IntConst(value.toLong, typeOf)
}
