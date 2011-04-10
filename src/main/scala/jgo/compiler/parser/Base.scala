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
  type PE[+T]  = Parser       [OrError[T]]
  type PPE[+T] = PackratParser[OrError[T]]
  type P[+T]   = Parser       [T]
  type PP[+T]  = Parser       [T]
  type P_      = Parser       [Any]
  type PP_     = PackratParser[Any]
  
  implicit def string2Fancy(str: String) = new FancyParserOps(str)
  
  
  lazy val identList: P[List[String]] =  "identifier list" $
    rep1sep(ident, ",")
  
  def repWithSemi[T](p: Parser[T]): Parser[List[T]] =
    ("repetition of "
    + p
    + " with semicolon; last semicolon optional"
    ) $
    repsep(p, ";") <~ ";".?
}