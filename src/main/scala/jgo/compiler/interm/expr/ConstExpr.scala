package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

import PartialFunction._ //cond is really useful. read the api.

/**
 * An expression whose value is determined at compile-time.
 * The public interface of the expr package permits the
 * direct creation of untyped constants, but not of typed
 * constants.  All typed constants are derived from
 * untyped constants at some point.
 */
sealed trait ConstExpr extends Expr {
  def valueString: String
}

/**
 * The constant expression corresponding to the value `nil`.
 */
object NilConst extends ConstExpr {
  val typeOf = NilType
  
  def eval      = PushNil
  def evalUnder = PushNil
  
  def valueString = "nil"
}

/**
 * A string constant.
 */
sealed trait StringConst extends ConstExpr {
  def value: String
  def valueString = "\"" + value + "\""
}

/**
 * A boolean constant.  Important: this trait does not extend,
 * and has nothing to do with, `BoolExpr`.  I plan to rename
 * `BoolExpr` to `ConditionalExpr`, to make this clearer.
 */
sealed trait BoolConst extends ConstExpr {
  def value: Boolean
  def valueString = value.toString
}

/**
 * A constant that is either complex, floating-point, or integral.
 */
sealed trait NumericConst extends ConstExpr {
  def real: BigDecimal
  def imag: BigDecimal
  
  def valueString =
    if (imag >= 0) "%s + %si" format (real, imag)
    else "%s - %si" format (real, -imag)
}

/**
 * A constant that is either floating-point or integral.
 */
sealed trait RealConst extends NumericConst {
  def real: BigDecimal
  val imag = BigDecimal(0) //for some reason, compiler refuses to apply implicit conversion
  
  override def valueString = real.toString
}

/**
 * An integral constant.
 */
sealed trait IntegralConst extends RealConst {
  def int: BigInt
  def real = BigDecimal(int)
  
  override def valueString = int.toString
}


object StringConst {
  def unapply(e: Expr): Option[String] =
    condOpt(e) { case s: StringConst => s.value }
}
object BoolConst {
  def unapply(e: Expr): Option[Boolean] =
    condOpt(e) { case b: BoolConst => b.value }
}
object NumericConst {
  def unapply(e: Expr): Option[(BigDecimal, BigDecimal)] =
    condOpt(e) { case n: NumericConst => (n.real, n.imag) }
}
object RealConst {
  def unapply(e: Expr): Option[BigDecimal] =
    condOpt(e) { case r: RealConst => r.real }
}
object IntegralConst {
  def unapply(e: Expr): Option[BigInt] =
    condOpt(e) { case i: IntegralConst => i.int }
}


/**
 * A constant whose type is fixed.
 */
private sealed trait TypedConst extends ConstExpr with EvalFromUnderlyingExpr {
  protected def requireOfUnderlying(pred: Type => Boolean) =
    require(pred(typeOf.underlying),
            "Impl error: %s has illegal underlying type %s".format(this, typeOf.underlying))
}

/**
 * A string constant whose type is that specified.
 */
private case class TypedStringConst(value: String, typeOf: Type) extends StringConst with TypedConst {
  requireOfUnderlying(_ == StringType)
  def evalUnder = PushStr(value)
}

/**
 * A boolean constant whose type is that specified.
 */
private case class TypedBoolConst(value: Boolean, typeOf: Type) extends BoolConst with TypedConst {
  requireOfUnderlying(_ == BoolType)
  def evalUnder = PushBool(value)
}

/**
 * A complex constant whose type is that specified.
 */
private case class TypedComplexConst(real: BigDecimal, imag: BigDecimal, typeOf: Type) extends NumericConst with TypedConst {
  requireOfUnderlying(_.isInstanceOf[ComplexType])
  def evalUnder = typeOf.underlying match {
    case ct: ComplexType => PushComplex(real, imag, ct)
  }
}

/**
 * A floating-point constant whose type is that specified.
 */
private case class TypedFloatingConst(real: BigDecimal, typeOf: Type) extends RealConst with TypedConst {
  requireOfUnderlying(_.isInstanceOf[FloatingType])
  def evalUnder = typeOf.underlying match {
    case ft: FloatingType => PushFloat(real, ft)
  }
}

/**
 * An integral constant whose type is that specified.
 */
private case class TypedIntegralConst(int: BigInt, typeOf: Type) extends IntegralConst with TypedConst {
  requireOfUnderlying(_.isInstanceOf[IntegralType])
  def evalUnder = typeOf.underlying match {
    case it: IntegralType => PushInt(int, it)
  }
}


/**
 * A constant whose type is not constrained.
 */
sealed trait UntypedConst extends ConstExpr {
  /**
   * The "type" of this untyped constant, which is defined by
   * `canFitIn` and `defaultType` methods.
   * We use a `lazy val` to avoid initialization problems;
   * if a subclass implemented `canFitIn` by performing some
   * checks on a field, that field would not be initialized
   * if we used a `val` here.
   */
  lazy val typeOf: Type = new UntypedConstType {
    def canFitIn(t: UnderType) = { println("can fit in"); UntypedConst.this.canFitIn(t) }
    def default = defaultType
  }
  
  /**
   * Gives rise to an UnsupportedOperationException, since the evaluation code
   * of a value with no type is not well-defined.
   */
  private[expr] def eval = throw new UnsupportedOperationException(
    "Internal impl error: attempting to call eval on untyped const " + this)
  
  /**
   * Gives rise to an UnsupportedOperationException, since the eval-underlying
   * code of a value with no type is not well-defined.
   */
  private[expr] def evalUnder = throw new UnsupportedOperationException(
    "Internal impl error: attempting to call evalUnderlying on untyped const " + this)
  
  /**
   * States whether this untyped constant may be considered a value of the
   * specified type.
   */
  def canFitIn(t: UnderType): Boolean
  
  /**
   * The type that should be inferred for variables initialized to this
   * untyped constant.
   */
  def defaultType: Type
  
  protected def withTypeUnchecked(t: Type): ConstExpr
  
  /**
   * Optionally produces a constant of the specified type whose value is
   * that of this constant, returning `None` if this constant's value
   * cannot fit in the underlying type of the specified type.
   */
  private[expr] final def withType(t: Type): Option[ConstExpr] = {
    val fits = cond(t.underlying) { case bt: BuiltinType => canFitIn(bt) }
    if (fits)
      Some(withTypeUnchecked(t))
    else
      None
  }
}

case class UntypedStringConst(value: String) extends StringConst with UntypedConst {
  def canFitIn(t: UnderType) = t == StringType
  def defaultType = scope.UniverseScope.string
  protected def withTypeUnchecked(t: Type): ConstExpr = TypedStringConst(value, t)
}

case class UntypedBoolConst(value: Boolean) extends BoolConst with UntypedConst {
  def canFitIn(t: UnderType) = t == BoolType
  def defaultType = scope.UniverseScope.bool
  protected def withTypeUnchecked(t: Type): ConstExpr = TypedBoolConst(value, t)
}

private object UntypedNumericUtils {
  def isBounded(lower: BigDecimal, v: BigDecimal, upper: BigDecimal) =
    lower <= v  && v <= upper
  
  def isFloat(v: BigDecimal): Boolean =
    isBounded(Float.MinValue, v, Float.MaxValue)
  
  def isDouble(v: BigDecimal): Boolean =
    isBounded(Double.MinValue, v, Double.MaxValue)
  
  def isBounded(lower: BigInt, v: BigInt, upper: BigInt) =
    lower <= v  && v <= upper
}
import UntypedNumericUtils._

/**
 * An untyped constant that is either complex, floating-point, or integral.
 */
sealed trait UntypedNumericConst extends NumericConst with UntypedConst {
  def canFitIn(t: UnderType) = cond(t) {
    case Complex64  => isFloat(real)  && isFloat(imag)
    case Complex128 => isDouble(real) && isDouble(imag)
  }
  protected def withTypeUnchecked(t: Type): ConstExpr = TypedComplexConst(real, imag, t)
}

/**
 * An untyped numeric constant that is either floating-point or integral.
 */
sealed trait UntypedRealConst extends RealConst with UntypedNumericConst {
  override def canFitIn(t: UnderType) = super.canFitIn(t) || cond(t) {
    case Float32 => isFloat(real)
    case Float64 => isDouble(real)
  }
  protected override def withTypeUnchecked(t: Type): ConstExpr = t.underlying match {
    case _: RealType => TypedFloatingConst(real, t)
    case _ => super.withTypeUnchecked(t)
  }
}

/**
 * An untyped complex constant.
 */
case class UntypedComplexConst(real: BigDecimal, imag: BigDecimal) extends UntypedNumericConst {
  def defaultType = scope.UniverseScope.complex128
}

/**
 * An untyped floating-point constant.
 */
case class UntypedFloatingConst(real: BigDecimal) extends UntypedRealConst {
  def defaultType = scope.UniverseScope.float64
}

/**
 * An untyped integral constant.
 */
case class UntypedIntegralConst(int: BigInt) extends IntegralConst with UntypedRealConst {
  override def canFitIn(t: UnderType) = super.canFitIn(t) || cond(t) {
    case Int8  => isBounded( Byte.MinValue, int,  Byte.MaxValue)
    case Int16 => isBounded(Short.MinValue, int, Short.MaxValue)
    case Int32 => isBounded(  Int.MinValue, int,   Int.MaxValue)
    case Int64 => isBounded( Long.MinValue, int,  Long.MaxValue)
    case Uint8  => isBounded(0, int,                   255)
    case Uint16 => isBounded(0, int,         Char.MaxValue)
    case Uint32 => isBounded(0, int,         (1 << 32) - 1)
    case Uint64 => isBounded(0, int, (BigInt(1) << 64) - 1)
  }
  def defaultType = scope.UniverseScope.int
  
  protected override def withTypeUnchecked(t: Type): ConstExpr = t.underlying match {
    case _: IntegralType => TypedIntegralConst(int, t)
    case _ => super.withTypeUnchecked(t)
  }
}


object UntypedNumericConst {
  def unapply(e: Expr): Option[(BigDecimal, BigDecimal)] = e match {
    case n: UntypedNumericConst => Some(n.real, n.imag)
    case _ => None
  }
}

object UntypedRealConst {
  def unapply(e: Expr): Option[BigDecimal] = e match {
    case r: UntypedRealConst => Some(r.real)
    case _ => None
  }
}
