package jgo.compiler
package parser

import combinatorExten._
import message._
//import lexer.{Scanner, Lexical}
import scope._

import scala.util.parsing._
import combinator._
import input.Reader
import input.{Position, NoPosition}

import scala.collection.mutable.ListBuffer

trait Base extends Tokens with PackratParsers with FancyParsers with MessageHandling {
  type P[+T]   = Parser       [T]
  type PP[+T]  = PackratParser[T]
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