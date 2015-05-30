package jgo.tools.compiler

sealed abstract class Message(prefix: String) {
  val msg: String
  val pos: Pos
  override def toString = pos.toString + " " + prefix + ": " + msg
  def longString = prefix + ": " + msg + "\n" + pos.longString
}

case class ErrorMsg  (msg: String, pos: Pos) extends Message("error")
case class WarningMsg(msg: String, pos: Pos) extends Message("warning")
case class NoteMsg   (msg: String, pos: Pos) extends Message("note")
