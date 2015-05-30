package jgo.tools.compiler
package interm
package types


/**
 * A marker trait that indicates that a type can be the type
 * of a compile-time constant.
 */
trait ConstableType extends Type

/**
 * A "type" for untyped constants.
 * 
 * It is intuitive to think of an untyped constant as belonging to a type
 * of its own -- a type specific to its value.  For example, the literal
 * `5` may be considered to have the type "integer with value 5", which is
 * distinct from the type "integer with value 6" of the literal `6`.
 * 
 * This is an approximation to the abstraction actually provided by this
 * trait.  Instead of associating a different type with each untyped value,
 * we choose to define the type of an untyped constant by which built-in types
 * that constant is compatible with (or, "can fit in") and what type should be
 * inferred for variables initialized to that constant (the "default type").
 * So the type of `5` is more properly "can fit in `int`, `uint`, `int8` - `int64`,
 * `uint8` - `uint64`, `float32`, `float64`, `complex64`, and `complex128`;
 * has default type `int`".
 */
trait UntypedConstType extends ConstableType with UnderType { //how oxymoronic!
  /**
   * Indicates whether this particular constant value
   * can fit in the specified type.
   */
  def canFitIn(t: UnderType): Boolean
  
  /**
   * The type that should be inferred for variables initialized to
   * a value of this type.
   * 
   * For example, consider the declaration `x := 42`.  The variable `x`
   * must be given a type, and it would be unreasonable for that type
   * to be "integer with value 42", since we would then be forbidden from
   * assigning it to anything other than `42`.
   * 
   * The Go spec provides rules that specify a "default inference type" (my term)
   * for untyped constants.  In our example above, the variable `x` would be
   * endowed with type `int`.
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
  def canFitIn(t: UnderType) =
    t.underlying == StringType
  
  def default = scope.UniverseScope.string
}

object UntypedBoolType extends UntypedConstType {
  def canFitIn(t: UnderType) =
    t.underlying == BoolType
  
  def default = scope.UniverseScope.string
}
