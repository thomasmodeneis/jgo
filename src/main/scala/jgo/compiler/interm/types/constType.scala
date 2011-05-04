package jgo.compiler
package interm
package types


/**
 * A marker trait that indicates that this type can be the type
 * of a compile-time constant.
 */
trait ConstableType extends Type

/**
 * A "type" for items (int literals, for example) whose effective
 * type is dependent on their value.
 */
trait UntypedConstType extends Constable { //how oxymoronic!
  /**
   * Indicates whether this particular constant value
   * can fit in the specified type.
   */
  def canFitIn(t: BuiltinType): Boolean
}
