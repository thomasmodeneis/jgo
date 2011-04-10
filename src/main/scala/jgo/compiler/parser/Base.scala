package jgo.compiler
package parser

import combinatorExten._
import message._
import lexer.{GoTokens, GoTokenScanners, GoLexical}
import scope._

import scala.util.parsing._
import combinator._
import input.Reader
import input.{Position, NoPosition}

import scala.collection.mutable.ListBuffer

trait Base extends GoTokenParsers with PackratParsers with FancyParsers with MessageHandling {
  override val lexical = new GoLexical
  type Tokens = GoLexical
  type P[+T]  = Parser       [OrError[PerhapsMsg[T]]]
  type PP[+T] = PackratParser[OrError[PerhapsMsg[T]]]
  type P_     = Parser       [OrError[PerhapsMsg[Any]]]
  type PP_    = PackratParser[OrError[PerhapsMsg[Any]]]
  
  
  lazy val identList: P[List[String]] =  "identifier list" $
    rep1sep(ident, ",")
  
  def repWithSemi[T](p: Parser[T]): Parser[List[T]] =
    ("repetition of "
    + p
    + " with semicolon; last semicolon optional"
    ) $
    repsep(p, ";") <~ ";".?
}