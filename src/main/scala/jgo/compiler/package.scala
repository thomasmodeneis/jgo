package jgo

import scala.util.parsing.input.{Position, NoPosition}

import jgo.compiler._
import message._

package object compiler {
  type M[+T] = Messaged[T]
  type Pos   = Position
  
  def together2[A, B]   (a: M[A], b: M[B])          = Messaged.together(a, b)
  def together3[A, B, C](a: M[A], b: M[B], c: M[C]) = Messaged.together(a, b, c)
}
