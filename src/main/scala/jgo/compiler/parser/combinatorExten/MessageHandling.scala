package jgo.compiler
package parser
package combinatorExten

import interm._
import util._
import message._

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{Positional, Position, NoPosition}
import scala.util.control.ControlThrowable

trait MessageHandling extends Parsers {
  
  implicit def withParser2certainParser[T](p: Parser[WithMsg[T]]): Parser[T] =
    positioned(p) ^^ { case WithMsg(m, cont) => addMsg(m); cont }
  
  implicit def any2noMsg[T](cont: T): PerhapsMsg[T] = NoMsg(cont)
  
  implicit def perhaps2cont[T](p: PerhapsMsg[T]): T = p match {
    case WithMsg(msg, cont) =>
      addMsg(msg)
      cont
    case NoMsg(cont) =>
      cont
  }
  
  implicit def any2orError[T](cont: T): OrError[T] = NoError(cont)
  
  implicit def orError2cont[T](o: OrError[T]): T = o match {
    case NoError(cont)     => cont
    case ErrorPresent(err) => throw ErrorThrow(err)
  }
  
  implicit def anyParser2orErrorParser[T](p: Parser[T]): Parser[OrError[T]] =
    p ^^ { NoError(_) }
  
  private var position: Position = NoPosition
  
  abstract override def Parser[T](f: Input => ParseResult[T]): Parser[T] = super.Parser {
    in => f(in) match {
      case r @ Success(_, in1) =>
        position = in1.pos
        r
      case r => r
    }
  }
  abstract override def OnceParser[T](f: Input => ParseResult[T]): OnceParser[T] =super.OnceParser {
    in => f(in) match {
      case r @ Success(_, in1) =>
        position = in1.pos
        r
      case r => r
    }
  }
  
  private def addMsg(msg: Message) {
    msg match {
        case e: ErrorMsg   => errs  ::= e
        case w: WarningMsg => warns ::= w
        case n: NoteMsg    => nts   ::= n
      }
  }
  
  /*
  def withErr  [T] (msg: String) (v: T): PerhapsMsg[T] = WithMsg(ErrorMsg(msg),   v)
  def withWarn [T] (msg: String) (v: T): PerhapsMsg[T] = WithMsg(WarningMsg(msg), v)
  def withNote [T] (msg: String) (v: T): PerhapsMsg[T] = WithMsg(NoteMsg(msg),    v)
  
  def withErrIf  [T] (cond: => Boolean, msg: String) (value: T): PerhapsMsg[T] =
    if (cond) withErr(msg)(value)  else NoMsg(value)
  def withWarnIf [T] (cond: => Boolean, msg: String) (value: T): PerhapsMsg[T] =
    if (cond) withWarn(msg)(value) else NoMsg(value)
  def withNoteIf [T] (cond: => Boolean, msg: String) (value: T): PerhapsMsg[T] =
    if (cond) withNote(msg)(value) else NoMsg(value)
  */
  
  /*def unless [T] (cond: => Boolean, msg: String) (res: T): OrError[T] =
    if (cond) ErrorPresent(ErrorMsg(msg)) else res*/
  
  def recordErr(msg: String, args: Any*) {
    val s = msg format (args: _*)
    errs ::= ErrorMsg(s).setPos(position)
  }
  def recordWarn(msg: String, args: Any*) {
    val s = msg format (args: _*)
    warns ::= WarningMsg(s).setPos(position)
  }
  def recordNote(msg: String, args: Any*) {
    val s = msg format (args: _*)
    nts ::= NoteMsg(s).setPos(position)
  }
  
  private case class ErrorThrow(msg: ErrorMsg) extends ControlThrowable
  
  def errIf(cond: => Boolean, msg: String) {
    if (cond) throw ErrorThrow(ErrorMsg(msg))
  }
  
  def handleErrors[T](p: Parser[OrError[T]]): Parser[Option[T]] = p ^^ {
    case ErrorPresent(err) => errs ::= err; None
    case NoError(cont)     => Some(cont)
  }

  /*Parser {
    in => try {
      p(in) map { Some(_) }
    } catch {
      case ErrorThrow(msg, in1) =>
        errs ::= msg //.setPos(in1.pos)
        Success(None, in1)
    }
  }*/
  
  class HandlingParserOps[+T](p: Parser[OrError[T]]) {
    /**
     * Produces a parser that results in None in the presence of a thrown error message
     * (not a thrown with-message; those are handled already), adding said message to
     * the error message list; it results in a Some containing the result of the given
     * parser otherwise. The '?' is intended to be evocative of the fact that the returned
     * parser has result-type Option[T].  This method's name should be pronounced "handle".
     */
    def !? = handleErrors(p)
  }
  implicit def parser2handlingOps[T](p: Parser[OrError[T]]) = new HandlingParserOps(p)
  
  class ErrorParserOps[+T](p: Parser[T]) {
    def ^^! [U] (f: T => U): Parser[OrError[U]] = positioned { p ^^ {
      t => try {
        NoError(f(t))
      } catch {
        case ErrorThrow(err) => ErrorPresent(err)
      }
    }}
  }
  implicit def parser2errorOps[T](p: Parser[T]) = new ErrorParserOps(p)
  
/*  @unchecked
  private def decorate [T] (f: Input => ParseResult[T]): Input => ParseResult[T] =
    in => try {
      f(in)
    } catch {
      case WithThrow(m, cont) => addMsg(m); cont.asInstanceOf[T]
      case UnposErrorThrow(m) => throw ErrorThrow(m, in.pos)
    }
  
  abstract override def Parser [T] (f: Input => ParseResult[T]): Parser[T] =
    super.Parser(decorate(f))
  abstract override def OnceParser [T] (f: Input => ParseResult[T]): Parser[T] =
    super.OnceParser(decorate(f)) */
  
  private var errs:  List[ErrorMsg]   = Nil
  private var warns: List[WarningMsg] = Nil
  private var nts:   List[NoteMsg]    = Nil
  
  def hasErrs: Boolean = !errs.isEmpty
  
  def errors   = errs  reverse
  def warnings = warns reverse
  def notes    = nts   reverse
  
  def messages =
    errs reverse_::: warns reverse_::: notes.reverse
  
 /* implicit def errParser2catParser [Cat <: Catenable[_, Cat]] (p: Parser[ErrorMsg]): Parser[Cat] =
    positioned(p) ^^ { err =>
      errs ::= err
      //Cat
    }*/
  //implicit def with2cat [A, Cat <: Catenable[A, Cat]] (w: WithMsg[Cat]): Cat = {
/*  implicit def with2any [To] (w: WithMessage[To]): To = {
    w match {
      case e: Error   =>  errs  ::= e
      case w: Warning =>  warns ::= e
      case n: Note    =>  nts   ::= e
    }
    w.cont
  }*/
}
