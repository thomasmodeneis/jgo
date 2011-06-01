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
 */
sealed trait ConstExpr extends Expr {
  def valueString: String
}

sealed trait TypedConst extends ConstExpr

/**
 * A constant whose type is not constrained.
 * It is intuitive to think of an untyped constant as belonging to a type
 * of its own -- a type specific to its value.  For example, the literal
 * `5` may be considered to have the type "integer with value 5", which is
 * distinct from the type "integer with value 6" of the literal `6`.
 */
sealed trait UntypedConst extends ConstExpr {
  val typeOf = new UntypedConstType {
    def canFitIn(t: BuiltinType) = UntypedConst.this.canFitIn(t)
    def default = defaultType
  }
  
  /**
   * Gives rise to an UnsupportedOperationException, since the evaluation code
   * of a value with no type is not well-defined.
   */
  private[expr] def eval = throw new UnsupportedOperationException(
    "Internal impl error: attempting to call eval on untyped const " + this)
  
  /**
   * States whether this untyped constant may be considered a value of the
   * specified type.
   */
  def canFitIn(t: BuiltinType): Boolean
  
  /**
   * The type that should be inferred for variables initialized to this
   * untyped constant.  For example, consider the declaration `x := 42`.
   * The variable `x` must be given a type, and it would be unreasonable
   * for that type to be "integer with value 42", since we would then be
   * forbidden from assigning it to anything other than `42`.  The Go spec
   * provides rules that specify a "default inference type" (my term) for
   * untyped constants.  In our example above, the variable `x` would be
   * endowed with type `int`.
   */
  def defaultType: Type
  
  protected def withTypeUnchecked(t: Type): Option[ConstExpr]
  
  private[expr] final def withType(t: Type): Option[ConstExpr] = {
    val fits = cond(t.underlying) {
      case bt: BuiltinType => canFitIn(bt)
    }
    if (fits) withTypeUnchecked(t)
    else None
  }
}

case class StringConst(value: String) extends ConstExpr {
  val typeOf = StringType
  def eval   = PushStr(value)
  
  def valueString = "\"" + value + "\""
}

case class BoolConst(value: Boolean) extends ConstExpr {
  val typeOf = BoolType
  def eval   = PushBool(value)
  
  def valueString = value.toString
}

private case class IntConst(value: Int) extends ConstExpr {
  val typeOf = Int32
  def eval   = PushInt(value, typeOf)
  
  def valueString = value.toString
}

private case class FloatConst(value: Double) extends ConstExpr {
  val typeOf = Float64
  def eval   = PushFloat(value, typeOf)
  
  def valueString = value.toString
}

/**
 * The constant expression corresponding to the value `nil`.
 */
object NilConst extends ConstExpr {
  val typeOf = NilType
  def eval   = PushNil
  
  def valueString = "nil"
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


/**
 * A complex constant whose type is that specified.
 */
private case class TypedComplexConst(real: BigDecimal, imag: BigDecimal, typeOf: Type) extends NumericConst with TypedConst {
  evalAs(typeOf) //trigger IllegalArgumentException upfront
  private def evalAs(t: Type): CodeBuilder = t match {
    case ct: ComplexType => PushComplex(real, imag, ct) //TODO: Add bounds check
    case wt: WrappedType => Wrap(wt) |+| evalAs(wt.unwrapped)
    case ta: TypeAlias   => evalAs(ta.effective)
    case _ => throw new IllegalArgumentException(
      "Impl error: illegal underlying type %s in %s" format (t, this))
  }
  def eval = evalAs(typeOf)
}

/**
 * A floating-point constant whose type is that specified.
 */
private case class TypedFloatingConst(real: BigDecimal, typeOf: Type) extends RealConst with TypedConst {
  evalAs(typeOf) //trigger IllegalArgumentException upfront
  private def evalAs(t: Type): CodeBuilder = t match {
    case ft: FloatingType => PushFloat(real, ft) //TODO: Add bounds check
    case wt: WrappedType  => Wrap(wt) |+| evalAs(wt.unwrapped)
    case ta: TypeAlias    => evalAs(ta.effective)
    case _ => throw new IllegalArgumentException(
      "Impl error: illegal underlying type %s in %s" format (t, this))
  }
  def eval = evalAs(typeOf)
}

/**
 * An integral constant whose type is that specified.
 */
private case class TypedIntegralConst(int: BigInt, typeOf: Type) extends IntegralConst with TypedConst {
  evalAs(typeOf) //trigger IllegalArgumentException upfront
  private def evalAs(t: Type): CodeBuilder = t match {
    case it: IntegralType => PushInt(int, it) //TODO: Add bounds check
    case wt: WrappedType  => Wrap(wt) |+| evalAs(wt.unwrapped)
    case ta: TypeAlias    => evalAs(ta.effective)
    case _ => throw new IllegalArgumentException(
      "Impl error: illegal underlying type %s in %s" format (t, this))
  }
  def eval = evalAs(typeOf)
}


/**
 * An untyped constant that is either complex, floating-point, or integral.
 */
sealed trait UntypedNumericConst extends NumericConst with UntypedConst {
  def canFitIn(t: BuiltinType) = cond(t) {
    case Complex64  => isFloat(real)  && isFloat(imag)
    case Complex128 => isDouble(real) && isDouble(imag)
  }
  protected def withTypeUnchecked(t: Type) =
    if (t.underlying.isInstanceOf[ComplexType]) Some(TypedComplexConst(real, imag, t))
    else None
}

/**
 * An untyped numeric constant that is either floating-point or integral.
 */
sealed trait UntypedRealConst extends RealConst with UntypedNumericConst {
  override def canFitIn(t: BuiltinType) = super.canFitIn(t) || cond(t) {
    case Float32 => isFloat(real)
    case Float64 => isDouble(real)
  }
  protected override def withTypeUnchecked(t: Type) =
    super.withTypeUnchecked(t) orElse {
      if (t.underlying.isInstanceOf[FloatingType]) Some(TypedFloatingConst(real, t))
      else None
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
  override def canFitIn(t: BuiltinType) = super.canFitIn(t) || cond(t) {
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
  
  protected override def withTypeUnchecked(t: Type) =
    super.withTypeUnchecked(t) orElse {
      if (t.underlying.isInstanceOf[IntegralType]) Some(TypedIntegralConst(int, t))
      else None
    }
}


object NumericConst {
  def unapply(e: Expr): Option[(BigDecimal, BigDecimal)] = e match {
    case n: NumericConst => Some(n.real, n.imag)
    case _ => None
  }
}

object RealConst {
  def unapply(e: Expr): Option[BigDecimal] = e match {
    case r: RealConst => Some(r.real)
    case _ => None
  }
}

object IntegralConst {
  def unapply(e: Expr): Option[BigInt] = e match {
    case i: IntegralConst => Some(i.int)
    case _ => None
  }
}


object UntypedNumericConst {
  def unapply(e: Expr): Option[(BigDecimal, BigDecimal)] = e match {
    case c: UntypedNumericConst => Some(c.real, c.imag)
    case _ => None
  }
}

object UntypedRealConst {
  def unapply(e: Expr): Option[BigDecimal] = e match {
    case c: UntypedRealConst => Some(c.real)
    case _ => None
  }
}
