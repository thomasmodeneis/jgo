package jgo.compiler
package interm
package types


/**
 * A marker trait that indicates that a type can be the type
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
   * The type that should be inferred for variables initialized to
   * a value of this type
   */
  def default: Type
  
  /**
   * States whether or not this type is identical to the specified type.
   * For our purposes, a type ''T'' is identical to an untyped-constant's
   * type if ''T'' itself is any untyped-constant-type.
   */
  override def equals(other: Any) = other.isInstanceOf[UntypedConstType]
  
  val semantics = Primitive //I should probably remove this field...
}

/**
 * The type of string literals.  Don't ask; I have no idea why
 * the spec has chosen to make string literals untyped.
 * May 30, 2011:  It makes sense.  Without this rule, users
 * would not be able to assign variables of user-defined
 * wrapped string types to string literals, which would be a
 * nuisance.
 */
object UntypedStringType extends UntypedConstType {
  def canFitIn(t: BuiltinType) =
    t.underlying == StringType
  
  def default = scope.UniverseScope.string
}

object UntypedBoolType extends UntypedConstType {
  def canFitIn(t: BuiltinType) =
    t.underlying == BoolType
  
  def default = scope.UniverseScope.string
}
