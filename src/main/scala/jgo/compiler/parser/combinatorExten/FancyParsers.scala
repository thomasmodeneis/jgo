package jgo.compiler
package parser.combinatorExten

import scala.util.parsing.combinator._
import scala.util.parsing.input.{Position, Positional}

/**
 * Provides useful parser combinators (p-combinators) and other extras
 * not implemented in the Scala parsing library.
 */
trait FancyParsers extends Parsers with ImplicitConversions {
  
  /**
   * A parser that returns the current position of the input and
   * consumes nothing.
   */
  object InPos extends Parser[Pos] {
    def apply(in: Input): ParseResult[Pos] = Success(in.pos, in)
  }
  
  /**
   * Converts the specified side-effectful action into a parser that performs
   * that action, consuming no input.
   * The intent of this conversion is to permit a clean syntax for semantic actions:
   * {{{
   * lazy val block =
   *   { pushScope() } ~> "{" ~> stmtList <~ "}" <~ { popScope() }
   * }}}
   */
  implicit def unit2parser(action: => Unit): Parser[Unit] = Parser { in =>
    Success(action, in)
  }
  
  class FancyParserOps[+T](p: Parser[T]) {
    /** Produces a parser committed in the second parser which discards the first result. */
    def ~>! [U] (q: => Parser[U]): Parser[U] = p ~> commit(q)
    /** Produces a parser committed in the second parser which discards the second result. */
    def <~! [U] (q: => Parser[U]): Parser[T] = p <~ commit(q)
    
    /**
     * Produces a parser that optionally applies this parser, indicating via result whether
     * or not it was applied.
     */
    def ?? : Parser[Boolean] = p.? ^^ { _.isDefined }
    
    def &@ (name: String): Parser[T] = nameize(p, name)
  }
  implicit def parser2Fancy[T](p: Parser[T]): FancyParserOps[T] = new FancyParserOps(p)
  
  /**
   * Produces a parser that performs the same actions as the specified parser
   * but returns the position of the input before the given parser is applied
   * instead of whatever the provided parser would.
   */
  def pos(p: Parser[_]): Parser[Position] = Parser { in =>
    p(in) map { _ => in.pos }
  }
  
  /**
   * Produces a parser that applies the specified one and tuples the
   * result with the position of the input.
   */
  def withPos[T](p: Parser[T]): Parser[(T, Pos)] = Parser { in =>
    p(in) map { res => (res, in.pos) }
  }
  
  /**
   * Produces a parser which applies the specified parser and whose result
   * is the input before that application.
   */
  def inputAt[T](p: Parser[T]): Parser[Input] = Parser { in =>
    p(in) map { _ => in }
  }
  
  /**
   * Provides a $-based syntax for naming parsers.
   * Example:
   * {{{
   * lazy val returnKeyword = "return keyword" $
   *   "return"
   * }}}
   */
  final class ParserName(val name: String) {
    /** Labels the specified parser with this name, returning that parser. */
    def $ [T] (p: Parser[T]): Parser[T] = nameize(p, name)
  }
  implicit def string2ParserName(name: String): ParserName = new ParserName(name)
  
  //We override nameize in TracePrintingParsers and ExceptionTracing.
  protected def nameize[T](p: Parser[T], name: String): Parser[T] = p named name
}