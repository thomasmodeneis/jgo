package jgo.compiler
package parser

import scala.util.parsing.combinator._
import lexer._
import scala.collection.mutable.HashMap

//this component includes code from, and corresponds to,
//scala.util.parsing.combinator.syntactical.StdTokenParsers
trait Tokens extends Parsers {
  /** So we can use the `pos` combinator. */
  self: Base =>
  
  type Elem = Token
  
  protected val keywordParserCache : HashMap[String, Parser[Pos]] = HashMap()

  /**
   * Produces a parser that accepts a Keyword containing the given string
   * (the provided string must actually be recognized as a keyword
   * by the lexer).  The result of the returned parser is the position in
   * the input where the keyword occurred.
   * 
   * @param chars    The character string making up the matched keyword.
   * @return a `Parser' that matches the given string
   */
  implicit def keyword(chars: String): Parser[Pos] =
    keywordParserCache.getOrElseUpdate(chars,
      Parser { in => in.first match {
        case Keyword(str) if str == chars => Success(in.pos, in.rest)
        case _ => Failure("`" + chars + "' expected", in)
      } }
    )
  
  /** A parser which matches an identifier */
  def ident: Parser[String] =
    elem("identifier", _.isInstanceOf[Identifier]) ^^ (_.chars)
  
  /** A parser which matches the specified identifier, returning its position */
  def ident(name: String): Parser[Pos] =
    pos(accept("the predeclared identifier " + name,
      { case Identifier(id) if id == name => id }))
  
  /** A parser which matches an integral literal */
  def intLit: Parser[BigInt] =
    accept("int literal", { case t: IntLit => t.value })
  
  /** A parser which matches a floating-point literal */
  def floatLit: Parser[BigDecimal] =
    accept("float literal", { case t: FloatLit => t.value })
  
  /** A parser which matches a character literal */
  def charLit: Parser[Int] =
    accept("char literal", { case t: CharLit => t.value })
  
  /** A parser which matches a string literal */
  def stringLit: Parser[String] =
    accept("string literal", { case t: StringLit => t.value })
}
