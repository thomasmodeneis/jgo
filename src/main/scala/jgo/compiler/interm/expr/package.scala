package jgo.compiler
package interm

import message._

import types._
import symbol._
import instr._
import instr.TypeConversions._
import codeseq._

import scala.util.parsing.input.Position

package object expr {
  private[expr] type M[+T] = Messaged[T]
  private[expr] type Pos   = Position
}
