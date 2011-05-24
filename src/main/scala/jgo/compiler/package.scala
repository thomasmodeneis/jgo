package jgo

import scala.util.parsing.input.{Position, NoPosition}
import scala.util.parsing.combinator._

package object compiler {
  type M[+T] = messaged.Messaged[T]
  type Pos   = Position
  
  def ordinal(n: Int): String = {
    require(n >= 0)
    (n: @scala.annotation.switch) match {
      case  0 => "zeroth"
      case  1 => "first"
      case  2 => "second"
      case  3 => "third"
      case  4 => "fourth"
      case  5 => "fifth"
      case  6 => "sixth"
      case  7 => "seventh"
      case  8 => "eighth"
      case  9 => "ninth"
      case 10 => "tenth"
      case 11 => "eleventh"
      case 12 => "twelfth"
      case 13 => "thirteenth"
      case 14 => "fourteenth"
      case 15 => "fifteenth"
      case 16 => "sixteenth"
      case 17 => "seventeenth"
      case 18 => "eighteenth"
      case 19 => "nineteenth"
      case 20 => "twentieth"
      case  i => i.toString + "th"
    }
  }
  
  
  /**
   * Wraps the provided value in a Result.
   * This method has result type M[T] so that
   * type inference behaves properly in cases like this:
   * {{{
   * (ls: List[M[Unit]]) => (ls foldLeft M(())) { _ then _ }
   * }}}
   */
  implicit def M[T](t: T): M[T] = messaged.Result(t)
  
  //val M = messaged.Messaged
  val Result = messaged.Result
  val Problem = messaged.Problem
  
  type Result[+T] = messaged.Result[T]
  type Problem = messaged.Problem
  
  @deprecated("use mTupled2 instead", "early May, 2011")
  def together2[A, B]   (a: M[A], b: M[B])          = messaged.Messaged.together(a, b)
  
  @deprecated("use mTupled3 instead", "early May, 2011")
  def together3[A, B, C](a: M[A], b: M[B], c: M[C]) = messaged.Messaged.together(a, b, c)
  
  implicit def mTupled2[A, B](v: (M[A], M[B])) = v match {
    case (a, b) => messaged.Messaged.together(a, b)
  }
  
  implicit def mTupled3[A, B, C](v: (M[A], M[B], M[C])) = v match {
    case (a, b, c) => messaged.Messaged.together(a, b, c)
  }
  
  implicit def mTupled4[A, B, C, D](v: (M[A], M[B], M[C], M[D])) = v match {
    case (a, b, c, d) => messaged.Messaged.together(a, b, c, d)
  }
  
  implicit def mTupled5[A, B, C, D, E](v: (M[A], M[B], M[C], M[D], M[E])) = v match {
    case (a, b, c, d, e) => messaged.Messaged.together(a, b, c, d, e)
  }
}
