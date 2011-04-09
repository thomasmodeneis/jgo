package jgo.compiler
package parser

import interm._
import util._
import message._

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.Parsers

trait MessageHandling {
  self: Parsers =>

  private var errs:  List[ErrorMsg] = Nil
  private var warns: List[Warning]  = Nil
  private var nts:   List[Note]     = Nil
  
  def hasErrs: Boolean = !errs.isEmpty
  
  def errors   = errs  reverse
  def warnings = warns reverse
  def notes    = nts   reverse
  
  def messages =
    errs reverse_::: warns reverse_::: notes.reverse
  
  implicit def withParser2certainParser [T] (p: Parser[WithMsg[T]]): Parser[T] =
    positioned(p) ^^ { case WithMsg(m, cont) =>
      m match {
        case e: ErrorMsg => errs  ::= e
        case w: Warning  => warns ::= w
        case n: Note     => nts   ::= n
      }
      cont
    }
  
  implicit def errParser2catParser [Cat <: Catenable[_, Cat]] (p: Parser[ErrorMsg]): Parser[Cat] =
    positioned(p) ^^ { err =>
      errs ::= err
      //Cat
    }
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
