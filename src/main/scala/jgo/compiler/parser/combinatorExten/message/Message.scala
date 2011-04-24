package jgo.compiler
package parser
package combinatorExten
package message

import scala.util.parsing.input.{Position, Positional}

sealed abstract trait PerhapsWithMsg[T]

sealed abstract class Message(prefix: String) extends Positional {
  val msg: String
  override def toString = pos.toString + " " + prefix + ": " + msg
  def longString = prefix + ": " + msg + "\n" + pos.longString
}

case class ErrorMsg  (msg: String) extends Message("error")
case class WarningMsg(msg: String) extends Message("warning")
case class NoteMsg   (msg: String) extends Message("note")
