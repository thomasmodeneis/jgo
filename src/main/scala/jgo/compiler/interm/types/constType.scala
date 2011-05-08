package jgo.compiler
package interm
package types


/**
 * A marker trait that indicates that this type can be the type
 * of a compile-time constant.
 */
trait ConstableType extends Type

/**
 * A "type" for items (int literals or untyped consts for example)
 * whose effective types are dependent on their values.
 */
trait UntypedConstType extends ConstableType { //how oxymoronic!
  /**
   * Indicates whether this particular constant value
   * can fit in the specified type.
   */
  def canFitIn(t: BuiltinType): Boolean
  
  /**
   * States whether or not this type is identical to the specified type.
   * For our purposes, a type ''T'' is identical to an untyped-constant's
   * type if ''T'' itself is any untyped-constant-type.
   */
  override def equals(other: Any) = other.isInstanceOf[UntypedConstType]
}

/**
 * The type of string literals.  Don't ask; I have no idea why
 * the spec has chosen to make string literals untyped.
 */
object UntypedStringType extends UntypedConstType {
  def canFitIn(t: BuiltinType) =
    t.underlying == StringType
}

object UntypedBoolType extends UntypedConstType {
  def canFitIn(t: BuiltinType) =
    t.underlying == BoolType
}
