package jgo.compiler
package parser

//import lexer.{Scanner, Lexical}
import message._
import scope._

import combinatorExten._

import scala.util.parsing._
import combinator._
import input.Reader
import input.{Position, NoPosition}

import scala.collection.mutable.ListBuffer

trait Base extends Tokens with PackratParsers with FancyParsers with MessageHandling {
  type Pos   = Position
  type M[+T] = message.Messaged[T]
  
  type P[+T]   = Parser       [T]
  type PP[+T]  = PackratParser[T]
  
  type PM[+T]  = Parser       [M[T]]
  type PPM[+T] = PackratParser[M[T]]
  
  type P_      = Parser       [Any]
  type PP_     = PackratParser[Any]
  
  implicit def string2Fancy(str: String) = new FancyParserOps(str)
  
  implicit def universalConv[A, B](p: Parser[A])(implicit ev: A => B): Parser[B] =
    p map ev
  
  lazy val identList: P[List[String]] =  "identifier list" $
    rep1sep(ident, ",")
  
  def repWithSemi[T](p: Parser[T]): Parser[List[T]] =
    ("repetition of "
    + p
    + " with semicolon; last semicolon optional"
    ) $
    repsep(p, ";") <~ ";".?
}