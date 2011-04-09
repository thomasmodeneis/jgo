package jgo.compiler.parser.combinatorExten

import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional

trait FancyParsers extends Parsers with ImplicitConversions {
  
  implicit def conv[A, B, C]         (f: (A, B) => C) =          flatten2(f)
  implicit def conv[A, B, C, D]      (f: (A, B, C) => D) =       flatten3(f)
  implicit def conv[A, B, C, D, E]   (f: (A, B, C, D) => E) =    flatten4(f)
  implicit def conv[A, B, C, D, E, F](f: (A, B, C, D, E) => F) = flatten5(f)
  
  class FancyParserOps[+T](p: Parser[T]) {
    def ~>! [U] (q: => Parser[U]): Parser[U] = p ~> commit(q)
    def <~! [U] (q: => Parser[U]): Parser[T] = p <~ commit(q)
    
    def ^?#  [U <: Positional] (f: PartialFunction[T, U]): Parser[U] =
      positioned(p ^? f)
    def ^?#  [U <: Positional] (f: PartialFunction[T, U], error: (T) ⇒ String): Parser[U] =
      positioned(p ^? (f, error))
    def ^^#  [U <: Positional] (f: T => U) = positioned(p ^^ f)
    def ^^^# [U <: Positional] (r: U) = positioned(p ^^^ r)
    
    def &# (implicit evidence: T <:< Positional): Parser[T] = positioned(p)
    
    def &@ (name: String): Parser[T] = nameize(p, name)
  }
  implicit def parser2Fancy[T](p: Parser[T]): FancyParserOps[String] = new FancyParserOps(p)
  implicit def string2Fancy(str: String):     FancyParserOps[String] = new FancyParserOps(str)
  
  
  protected def nameize[T](p: Parser[T], name: String): Parser[T] = p named name
  final class ParserName(val name: String) {
    def $ [T] (p: Parser[T]): Parser[T] = nameize(p, name)
  }
  implicit def string2ParserName(name: String): ParserName = new ParserName(name)
  
//  def parser[T](parsingFunc: PartialFunction[Input, T]) = new Parser[T] {
//    def apply(in: Input): ParseResult[T] = (parsingFunc orElse err("Error: illegal construct")).apply(in)
//  }
}