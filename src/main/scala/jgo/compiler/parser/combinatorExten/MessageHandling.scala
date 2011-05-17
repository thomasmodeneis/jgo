package jgo.compiler
package parser
package combinatorExten

import jgo.compiler.interm._
import jgo.compiler.util._

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{Positional, Position, NoPosition}
import scala.util.control.ControlThrowable

trait MessageHandling extends Parsers {
  private var position: Position = NoPosition
  
  private var errs:  List[ErrorMsg]   = Nil
  private var warns: List[WarningMsg] = Nil
  private var nts:   List[NoteMsg]    = Nil
  
  private def addMsg(msg: Message): Unit = msg match {
      case e: ErrorMsg   => errs  ::= e
      case w: WarningMsg => warns ::= w
      case n: NoteMsg    => nts   ::= n
  }
  
  def recordErr(msg: String, args: Any*) {
    val s = msg.format(args: _*)
    errs ::= ErrorMsg(s).setPos(position)
  }
  
  def recordWarn(msg: String, args: Any*) {
    val s = msg.format(args: _*)
    warns ::= WarningMsg(s).setPos(position)
  }
  
  def recordNote(msg: String, args: Any*) {
    val s = msg.format(args: _*)
    nts ::= NoteMsg(s).setPos(position)
  }
  
  def hasErrs: Boolean = !errs.isEmpty
  
  def errors   = errs  reverse
  def warnings = warns reverse
  def notes    = nts   reverse
  
  def messages = errs reverse_::: warns reverse_::: notes.reverse
  
  
  private def wrap[T](f: Input => ParseResult[T]): Input => ParseResult[T] =
    in => f(in) match {
      case r @ Success(_, in1) =>
        position = in1.pos
        r
      case r => r
    }
  
  /*
  abstract override def Parser[T](f: Input => ParseResult[T]): Parser[T] =
    super.Parser(wrap(f))
  
  abstract override def OnceParser[T](f: Input => ParseResult[T]): OnceParser[T] =
    super.OnceParser(wrap(f))
  */
}
