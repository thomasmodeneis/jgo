package jgo.compiler
package util

import scala.collection._

/**
 * This trait describes something pretty similar to a monoid.
 * More formally, for all valid A, CatTo, the set of all the values
 * of all the types T <: Catenable[A, CatTo] is a monoid under |+|.
 */
trait Catenable[+A, CatTo <: Catenable[A, CatTo]] {
  def |+| (other: CatTo): CatTo
  
  /**
   * The zero of Catenable[A, CatTo].
   */
  def catZero: Catenable[A, CatTo]
}

abstract class ImplicitCatZeroProvider[Cat <: Catenable[_, Cat]] {
  def catZero: Catenable[_, Cat]
}
