package jgo

import scala.util.parsing.input.{Position, NoPosition}

import jgo.compiler._
import message._

package object compiler {
  type M[+T] = Messaged[T]
  type Pos   = Position
  
  /**
   * Wraps the provided value in a Result.
   * This method has result type M[T] so that
   * type inference behaves properly in cases like this:
   * {{{
   * (ls: List[M[Unit]]) => (ls foldLeft M(())) { _ then _ }
   * }}}
   */
  implicit def M[T](t: T): M[T] = Result(t)
  
  def together2[A, B]   (a: M[A], b: M[B])          = Messaged.together(a, b)
  def together3[A, B, C](a: M[A], b: M[B], c: M[C]) = Messaged.together(a, b, c)
}
