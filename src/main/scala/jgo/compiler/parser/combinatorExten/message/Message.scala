package jgo.compiler
package parser
package combinatorExten
package message

import scala.util.parsing.input.{Position, Positional}

sealed abstract trait PerhapsWithMsg[T]

sealed abstract class Message(val msg: String) extends Positional

case class ErrorMsg  (msg: String) extends Message(msg)
case class WarningMsg(msg: String) extends Message(msg)
case class NoteMsg   (msg: String) extends Message(msg)
