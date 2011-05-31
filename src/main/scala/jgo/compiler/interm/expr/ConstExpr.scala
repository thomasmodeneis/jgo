package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

import PartialFunction._

/**
 * An expression whose value is determined at compile-time.
 */
sealed trait ConstExpr extends Expr

sealed trait UntypedConst extends ConstExpr {
  val typeOf = new UntypedConstType { def canFitIn(t: BuiltinType) = UntypedConst.this.canFitIn(t) }
  def eval = throw new UnsupportedOperationException(
    "Internal impl error: attempting to call eval on untyped const " + this)
  
  protected def canFitIn(t: BuiltinType): Boolean
}

case class StringConst(value: String) extends ConstExpr {
  val typeOf = StringType
  def eval   = PushStr(value)
}

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

/**
 * The expression corresponding to the value `nil`.
 */
object NilConst extends ConstExpr {
  val typeOf = NilType
  def eval   = PushNil
}


private object UntypedUtils {
  def isBounded(lower: BigDecimal, v: BigDecimal, upper: BigDecimal) =
    lower <= v  && v <= upper
  
  def isFloat(v: BigDecimal): Boolean =
    isBounded(Float.MinValue, v, Float.MaxValue)
  
  def isDouble(v: BigDecimal): Boolean =
    isBounded(Double.MinValue, v, Double.MaxValue)
  
  def isBounded(lower: BigInt, v: BigInt, upper: BigInt) =
    lower <= v  && v <= upper
}
import UntypedUtils._

sealed trait UntypedComplexConst extends UntypedConst {
  def real: BigDecimal
  def imag: BigDecimal
  
  protected def canFitIn(t: BuiltinType) = cond(t) {
    case Complex64  => isFloat(real)  && isFloat(imag)
    case Complex128 => isDouble(real) && isDouble(imag)
  }
}

sealed trait UntypedRealConst extends UntypedComplexConst {
  def real: BigDecimal
  val imag = BigDecimal(0) //for some reason, compiler refuses to apply implicit conversion
  
  protected override def canFitIn(t: BuiltinType) = super.canFitIn(t) || cond(t) {
    case Float32 => isFloat(real)
    case Float64 => isDouble(real)
  }
}

sealed trait IntegralConst extends ConstExpr {
  def int: BigInt
}

case class UntypedIntegralConst(int: BigInt) extends IntegralConst with UntypedRealConst {
  def real = BigDecimal(int)
  
  protected override def canFitIn(t: BuiltinType) = super.canFitIn(t) || cond(t) {
    case Int8   => isBounded( Byte.MinValue, int,  Byte.MaxValue)
    case Int16  => isBounded(Short.MinValue, int, Short.MaxValue)
    case Int32  => isBounded(  Int.MinValue, int,   Int.MaxValue)
    case Int64  => isBounded( Long.MinValue, int,  Long.MaxValue)
    
    case Uint8  => isBounded(0, int,                   255)
    case Uint16 => isBounded(0, int,         Char.MaxValue)
    case Uint32 => isBounded(0, int,         (1 << 32) - 1)
    case Uint64 => isBounded(0, int, (BigInt(1) << 64) - 1)
  }
}

object UntypedComplexConst extends ((BigDecimal, BigDecimal) => UntypedComplexConst) {
  def apply(r: BigDecimal, i: BigDecimal) =
    new UntypedComplexConst {
      val real = r
      val imag = i
    }
  def unapply(e: Expr): Option[(BigDecimal, BigDecimal)] = e match {
    case c: UntypedComplexConst => Some(c.real, c.imag)
    case _ => None
  }
}

object UntypedRealConst extends (BigDecimal => UntypedRealConst) {
  def apply(r: BigDecimal) = new UntypedRealConst { val real = r }
  def unapply(e: Expr): Option[BigDecimal] = e match {
    case c: UntypedRealConst => Some(c.real)
    case _ => None
  }
}

object IntegralConst {
  def unapply(e: Expr): Option[BigInt] = e match {
    case c: IntegralConst => Some(c.int)
    case _ => None
  }
}
