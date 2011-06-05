package jgo

import scala.util.parsing.input.{Position, NoPosition}
import scala.util.parsing.combinator._

package object compiler {
  type Pos = Position
  
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
  
  def result[T](v: T): Err[T] = Result(v)
  
  def problem[T](msg: String, args: Any*)(implicit pos: Pos): Err[T] =
    Problems.one(msg, args)(pos)
  
  /**
   * Wraps the provided value in a Result.
   * This method has result type Err[T] so that
   * type inference behaves properly in cases like this:
   * {{{
   * (ls: List[Err[Unit]]) => (ls foldLeft M(())) { _ then _ }
   * }}}
   */
  //implicit def Err[T](t: T): Err[T] = messaged.Result(t)
  
  implicit def errTupled2[A, B](v: (Err[A], Err[B])) = v match {
    case (a, b) => Err.together(a, b)
  }
  
  implicit def errTupled3[A, B, C](v: (Err[A], Err[B], Err[C])) = v match {
    case (a, b, c) => Err.together(a, b, c)
  }
  
  implicit def errTupled4[A, B, C, D](v: (Err[A], Err[B], Err[C], Err[D])) = v match {
    case (a, b, c, d) => Err.together(a, b, c, d)
  }
  
  implicit def errTupled5[A, B, C, D, E](v: (Err[A], Err[B], Err[C], Err[D], Err[E])) = v match {
    case (a, b, c, d, e) => Err.together(a, b, c, d, e)
  }
}
