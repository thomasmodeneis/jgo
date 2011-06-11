package jgo.tools.compiler
package parser
package combinatorExten

import jgo.tools.compiler.util._

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.NoPosition

trait MessageHandling extends Parsers {
  private var position: Pos = NoPosition
  
  private var errs:  List[ErrorMsg]   = Nil
  private var warns: List[WarningMsg] = Nil
  private var nts:   List[NoteMsg]    = Nil
  
  private def addMsg(msg: Message): Unit = msg match {
      case e: ErrorMsg   => errs  ::= e
      case w: WarningMsg => warns ::= w
      case n: NoteMsg    => nts   ::= n
  }
  
  def recordErr(msg: String, args: Any*)(pos: Pos) {
    val s = msg.format(args: _*)
    errs ::= ErrorMsg(s, pos)
  }
  
  def recordWarn(msg: String, args: Any*)(pos: Pos) {
    val s = msg.format(args: _*)
    warns ::= WarningMsg(s, pos)
  }
  
  def recordNote(msg: String, args: Any*)(pos: Pos) {
    val s = msg.format(args: _*)
    nts ::= NoteMsg(s, pos)
  }
  
  def hasErrs: Boolean = !errs.isEmpty
  
  def errors   = errs.reverse
  def warnings = warns.reverse
  def notes    = nts.reverse
  
  def messages = errs reverse_::: warns reverse_::: notes.reverse
  
  
  private def wrap[T](f: Input => ParseResult[T]): Input => ParseResult[T] =
    in => f(in) match {
      case r @ Success(_, in1) =>
        position = in1.pos
        r
      case r => r
    }
}
