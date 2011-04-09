package jgo.compiler
package parser

import scala.util.parsing.combinator._
import syntactical._
import lexer.GoTokens
import scala.collection.mutable.HashMap

//this component includes code from, and corresponds to,
//scala.util.parsing.combinator.syntactical.StdTokenParsers
trait GoTokenParsers extends TokenParsers {
  type Tokens <: lexer.GoTokens
  import lexical.{Token, Keyword, Identifier, IntLit, FloatLit, CharLit, StringLit}
  
  protected val keywordCache : HashMap[String, Parser[String]] = HashMap.empty

  /** A parser which matches a single keyword token.
   *
   * @param chars    The character string making up the matched keyword.
   * @return a `Parser' that matches the given string
   */
  implicit def keyword(chars: String): Parser[String] = {
    //println("   looking for `" + chars + "'")
    keywordCache.getOrElseUpdate(chars, accept("`" + chars + "'", { case Keyword(str) if str == chars => chars }))
  }
  
  /** A parser which matches an identifier */
  def ident: Parser[String] = {
    //println("   looking for an ident")
    elem("identifier", _.isInstanceOf[Identifier]) ^^ (_.chars)
  }
  
  def ident(name: String): Parser[String] = {
    //println("   looking for the predeclared ident `" + name + "'")
    accept("the predeclared identifier " + name,
      { case Identifier(id) if id == name => id })
  }
  
  /** A parser which matches an integral literal */
  def intLit: Parser[IntLit] = {
    //println("   looking for an int literal")
    accept("int literal", { case t: IntLit => t })
  }
  
  /** A parser which matches a floating-point literal */
  def floatLit: Parser[FloatLit] = {
    //println("   looking for a float literal")
    accept("float literal", { case t: FloatLit => t })
  }
  
  /** A parser which matches a character literal */
  def charLit: Parser[CharLit] = {
    //println("   looking for a char literal")
    accept("char literal", { case t: CharLit => t })
  }
  
  /** A parser which matches a string literal */
  def stringLit: Parser[StringLit] = {
    //println("   looking for a string literal")
    accept("string literal", { case t: StringLit => t })
  }
}
