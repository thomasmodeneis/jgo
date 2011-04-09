package jgo.compiler
package interm
package types


/**
 * A marker trait that indicates that this type can be the type
 * of a compile-time constant.
 */
trait Constable extends Type

/**
 * A marker trait that signifies that the value in question is
 * a compile-time constant.
 */
trait Const extends Type {
  self: Constable =>
}

/**
 * 
 */
trait UntypedConst extends Constable with Const { //how oxymoronic!
  /**
   * Indicates whether this particular constant value
   * can fit in the specified type.
   */
  def canFitIn(t: BuiltinType): Boolean
}
