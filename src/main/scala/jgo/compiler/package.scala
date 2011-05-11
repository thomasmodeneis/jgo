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
  
  @deprecated("use mTupled2 instead")
  def together2[A, B]   (a: M[A], b: M[B])          = Messaged.together(a, b)
  
  @deprecated("use mTupled3 instead")
  def together3[A, B, C](a: M[A], b: M[B], c: M[C]) = Messaged.together(a, b, c)
  
  implicit def mTupled2[A, B](v: (M[A], M[B])) = v match {
    case (a, b) => Messaged.together(a, b)
  }
  
  implicit def mTupled3[A, B, C](v: (M[A], M[B], M[C])) = v match {
    case (a, b, c) => Messaged.together(a, b, c)
  }
  
  implicit def mTupled4[A, B, C, D](v: (M[A], M[B], M[C], M[D])) = v match {
    case (a, b, c, d) => Messaged.together(a, b, c, d)
  }
  
  implicit def mTupled5[A, B, C, D, E](v: (M[A], M[B], M[C], M[D], M[E])) = v match {
    case (a, b, c, d, e) => Messaged.together(a, b, c, d, e)
  }
}
