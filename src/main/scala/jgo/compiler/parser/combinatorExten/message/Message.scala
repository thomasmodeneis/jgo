package jgo.compiler
package parser
package combinatorExten
package message

import scala.util.parsing.input.{Position, Positional}

sealed abstract trait PerhapsWithMsg[T]

sealed abstract class Message extends Positional {
  val msg: String
}

case class ErrorMsg  (msg: String) extends Message
case class WarningMsg(msg: String) extends Message
case class NoteMsg   (msg: String) extends Message
