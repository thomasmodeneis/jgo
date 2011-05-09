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

trait Base extends Tokens with PackratParsers with FancyParsers with MessageHandling {
  
  type P[+T]   = Parser       [T]
  type PP[+T]  = PackratParser[T]
  
  type PM[+T]  = Parser       [M[T]]
  type PPM[+T] = PackratParser[M[T]]
  
  type P_      = Parser       [Any]
  type PP_     = PackratParser[Any]
  
  implicit def string2Fancy(str: String) = new FancyParserOps(str)
  
  //implicit def res2Msgd[T](t: T): M[T] = Messaged.res2Msgd(t)
  implicit def liftList[T](ms:  List[M[T]]):   M[List[T]]   = Messaged.lsM2mLs(ms)
  implicit def liftOpt [T](opt: Option[M[T]]): M[Option[T]] = Messaged.optM2mOpt(opt)
  
  implicit def parserConv[A, A1](p: Parser[A])(implicit ev: A => A1): Parser[A1] =
    p ^^ ev
  
  implicit def leftConv[A, A1, B](v: A ~ B)(implicit ev: A => A1): A1 ~ B = v match {
    case a ~ b => new ~(ev(a), b)
  }
  
  implicit def rightConv[A, B, B1](v: A ~ B)(implicit ev: B => B1): A ~ B1 = v match {
    case a ~ b => new ~(a, ev(b))
  }
  
  implicit def liftList[T](p: Parser[List[M[T]]]): Parser[M[List[T]]] =
    p ^^ Messaged.lsM2mLs
  
  implicit def liftOpt[T](p: Parser[Option[M[T]]]): Parser[M[Option[T]]] =
    p ^^ Messaged.optM2mOpt
  
  
  lazy val identList: P[List[String]] =  "identifier list" $
    rep1sep(ident, ",")
  
  def repWithSemi[T](p: Parser[T]): Parser[List[T]] =
    ("repetition of "
    + p
    + " with semicolon; last semicolon optional"
    ) $
    repsep(p, ";") <~ ";".?
}