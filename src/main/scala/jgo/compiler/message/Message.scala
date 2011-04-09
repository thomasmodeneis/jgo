package jgo.compiler
package message

import scala.util.parsing.input.{Position, Positional}

sealed abstract class Message(val msg: String) extends Positional

case class ErrorMsg(msg: String) extends Message(msg)
case class Warning (msg: String) extends Message(msg)
case class Note    (msg: String) extends Message(msg)
