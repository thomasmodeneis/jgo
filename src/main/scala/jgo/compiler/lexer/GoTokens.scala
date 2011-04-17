package jgo.compiler
package lexer

import scala.util.parsing._
import combinator._
import token._

trait GoTokens extends Tokens {
  case class Keyword(override val chars: String) extends Token
  
  case class Identifier(name: String) extends Token {
    override def chars = name
  }
  
  object IntLit {
    def apply(v: Long): IntLit = new IntLit(v.toString, v, 10)
    def apply(v: Int):  IntLit = new IntLit(v.toString, v, 10)
    
    def apply(chars: String, radix: Int): Token = { //might be error token
      require(radix == 10 || radix == 16 || radix == 2 || radix == 8)
      val value: Option[BigInt] = try radix match {
        case 2 =>
          if (chars(0) == '0' && chars(1).toLower == 'b')
            Some(BigInt(chars drop 2, 2))
          else None
        
        case 8 =>
          if (chars(0) == '0') //pass the 0 along to parseLong; it can't do any harm. But if we don't
            Some(BigInt(chars, 8))//the int literal "0" will become BigInt("", 8) -> error
          else None
        
        case 10 =>
          if (chars(0) != '0')
            Some(BigInt(chars, 10))
          else None
        
        case 16 =>
          if (chars(0) == '0' && chars(1).toLower == 'x')
            Some(BigInt(chars drop 2, 16))
          else None
      }
      catch {
        case e: NumberFormatException => None
      }
      if (value.isDefined)
        new IntLit(chars, value.get, radix)
      else
        ErrorToken("Invalid integer literal: " + chars)
    }
    def unapply(tok: Token): Option[BigInt] = tok match {
      case intLit: IntLit => Some(intLit.value)
      case _ => None
    }
  }
  class IntLit private (override val chars: String, val value: BigInt, val radix: Int) extends Token {
    override def toString = "integral literal " + chars //+
      (if (radix != 10) " (value = " + value + ")" else "")
  }
  
  object FloatLit {
    def apply(chars: String): Token =
      try new FloatLit(chars, BigDecimal(chars))
      catch {
        case e: NumberFormatException => ErrorToken("Invalid floating-point literal: " + chars)
      }
    
    def unapply(tok: Token): Option[BigDecimal] = tok match {
      case f: FloatLit => Some(f.value)
      case _ => None
    }
  }
  
  class FloatLit private (override val chars: String, val value: BigDecimal) extends Token {
    override def toString = "floating-point literal: " + chars
  }
  
  //behold! the correct impl for all the fancy-schmancy unicode nonsense! (I hope)
  object CharLit {
    def apply(str: String): Token =
      try { //Could be ErrorToken!!!
        characterize(str) map {
          ch => new CharLit(java.lang.Character.toChars(ch).mkString, "'" + str + "'")
        } getOrElse ErrorToken("Bad char literal: '" + str +"'")
      } catch {
        case e: IllegalArgumentException => ErrorToken("Invalid unicode code point in char literal '" + str + "'")
    }
    
    def unapply(tok: Token): Option[String] = tok match {
      case charLit: CharLit => Some(charLit.chars)
      case _ => None
    }
  }
  class CharLit private(val value: String, override val chars: String) extends Token {
    override def toString = "character literal: " + chars
  }
  
  object StringLit {
    def raw(chars: String): StringLit = new StringLit(chars, "`" + chars + "`", true)
    def interp(chars: String): Token = { //forgive me for the imperative style. :(
      var ls = chars.toList
      val sb = new StringBuilder
      while (!ls.isEmpty) {
        val (pref, suf) = ls span (_ != '\\')
        sb ++= pref
        if (!suf.isEmpty) {
          val (fancyEscape, afterEsc) = suf(1) match {
            case x if x.toLower == 'x' =>
              suf splitAt 4
            case digit if digit.isDigit =>
              suf splitAt 4
            case 'u' =>
              suf splitAt 5
            case 'U' =>
              suf splitAt 9
            case _ => // \n, etc
              suf splitAt 2
          }
          val escStr = fancyEscape.mkString
          CharLit(escStr) match {
            case ErrorToken(_) =>
              return ErrorToken("Invalid escape sequence '" + escStr +
                "' in interpreted string literal \"" + chars + "\"")
            case ch @ CharLit(_) =>
              sb ++= ch.chars
              ls = afterEsc
          }
        }
        else
          ls = Nil //or, ls = suf, which must be empty if we're here
      }
      new StringLit(sb.result, "\"" + chars + "\"", false)
    }
    def unapply(tok: Token): Option[String] = tok match {
      case stringLit: StringLit => Some(stringLit.value)
      case _ => None
    }
  }
  class StringLit private(val value: String, override val chars: String, val isRaw: Boolean) extends Token {
    override def toString =
      if (isRaw) "raw string literal: " + chars
      else "interpreted string literal: " + chars
  }
  
  private def characterize(str: String): Option[Int] = {
    //println("characterizing sequence: " + str)
    if (str.length == 0)
      None
    else if (str(0) != '\\')
      if (str.length != 1)
        None
      else
        Some(str.codePointAt(0))
    else try { str(1) match {
      case x if x.toLower == 'x' =>
        if (str.length != 4) //as in: \xFF
          None
        else
         Some(java.lang.Long.parseLong(str.substring(2, 4), 16).asInstanceOf[Int]) //2 is the index of the char after x
      
      case digit if digit.isDigit =>
        if (str.length != 4) //as in: \377
          None
        else
          Some(java.lang.Long.parseLong(str.substring(1, 4), 8).asInstanceOf[Int]) //1 is the index of the char after \
      
      case 'u' =>
        if (str.length != 6) //as in: \uABCD
          None
        else
          Some(java.lang.Long.parseLong(str.substring(2, 6), 16).asInstanceOf[Int]) //2 is the index of the char after u
      
      case 'U' =>
        if (str.length != 10) //as in: \uAABBCCDD
          None
        else
          Some(java.lang.Long.parseLong(str.substring(2, 10), 16).asInstanceOf[Int]) //2 is the index of the char after U
      
      case 'a'  => if (str.length == 2) Some('\u0007') else None
      case 'b'  => if (str.length == 2) Some('\b')     else None
      case 'f'  => if (str.length == 2) Some('\f')     else None
      case 'n'  => if (str.length == 2) Some('\n')     else None
      case 'r'  => if (str.length == 2) Some('\r')     else None
      case 't'  => if (str.length == 2) Some('\t')     else None
      case 'v'  => if (str.length == 2) Some('\u000b') else None
      case '\\' => if (str.length == 2) Some('\\')     else None
      case '\'' => if (str.length == 2) Some('\'')     else None
      case '"'  => if (str.length == 2) Some('"')      else None  //why not support '\"' if it makes my life easier?
      case _ => None
    } }
    catch {
      case e: NumberFormatException =>
        //println("Uh-oh.  A NumberFormatException for characterize('" + str + "')")
        None
    }
  }
  
  
}
