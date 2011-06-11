package jgo.tools.compiler
package interm
package types

/**
 * A type that may be an underlying type.
 * We use the term ''under-type'' to refer to such a type so as
 * to minimize confusion in sentences such as: "The underlying type
 * of any type must be an under-type."
 * 
 * More formally, we call any type whose underlying type is itself
 * an under-type.  Named types are the only types that are ''not''
 * under-types.
 */
trait UnderType extends Type {
  /**
   * The underlying type of this under-type, which by definition
   * is this type itself.
   */
  def underlying: UnderType = this
  
  /**
   * The effective type of this under-type is this type itself.
   */
  def effective: Type = this
}
