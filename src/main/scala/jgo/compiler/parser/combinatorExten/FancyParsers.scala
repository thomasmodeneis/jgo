package jgo.compiler
package parser.combinatorExten

import scala.util.parsing.combinator._
import scala.util.parsing.input.{Position, Positional}

trait FancyParsers extends Parsers with ImplicitConversions {
  /*
  implicit def conv[A, B, C]         (f: (A, B) => C) =          flatten2(f)
  implicit def conv[A, B, C, D]      (f: (A, B, C) => D) =       flatten3(f)
  implicit def conv[A, B, C, D, E]   (f: (A, B, C, D) => E) =    flatten4(f)
  implicit def conv[A, B, C, D, E, F](f: (A, B, C, D, E) => F) = flatten5(f)
  */
  
  object InPos extends Parser[Pos] {
    def apply(in: Input): ParseResult[Pos] = Success(in.pos, in)
  }
  
  class FancyParserOps[+T](p: Parser[T]) {
    def ~>! [U] (q: => Parser[U]): Parser[U] = p ~> commit(q)
    def <~! [U] (q: => Parser[U]): Parser[T] = p <~ commit(q)
    
    def >*> [U] (fp: Parser[T => U]): Parser[U] = Parser { in0 =>
      p(in0) flatMapWithNext { t => in =>
        fp(in) map { f => f(t) }
      }
    }
    
    def &@ (name: String): Parser[T] = nameize(p, name)
  }
  implicit def parser2Fancy[T](p: Parser[T]): FancyParserOps[T]      = new FancyParserOps(p)
//  implicit def string2parser(str: String):    Parser[String]        
  
  def pos(p: Parser[_]): Parser[Position] = Parser { in =>
    p(in) map { _ => in.pos }
  }
  
  def withPos[T](p: Parser[T]): Parser[(T, Pos)] = Parser { in =>
    p(in) map { res => (res, in.pos) }
  }
  
  //We override nameize in TracePrintingParsers.
  protected def nameize[T](p: Parser[T], name: String): Parser[T] = p named name
  final class ParserName(val name: String) {
    def $ [T] (p: Parser[T]): Parser[T] = nameize(p, name)
  }
  implicit def string2ParserName(name: String): ParserName = new ParserName(name)
  
//  def parser[T](parsingFunc: PartialFunction[Input, T]) = new Parser[T] {
//    def apply(in: Input): ParseResult[T] = (parsingFunc orElse err("Error: illegal construct")).apply(in)
//  }
}