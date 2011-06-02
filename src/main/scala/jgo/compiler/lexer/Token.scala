package jgo.compiler
package lexer

import scala.annotation.{switch, tailrec}

import Token._

sealed abstract class Token {
  val chars: String
  override def toString = chars
}

case object EOF extends Token {
  val chars = "<EOF>"
}

case class ErrorToken(msg: String) extends Token {
  val chars = "<error: " + msg + ">"
}

case class Keyword(chars: String) extends Token

case class Identifier(name: String) extends Token {
  val chars = name
  override def toString = "`" + chars + "'"
}

class IntLit private(val chars: String, val value: BigInt, val radix: Int) extends Token
object IntLit {
  def apply(v: Long): IntLit = new IntLit(v.toString, v, 10)
  def apply(v: Int):  IntLit = new IntLit(v.toString, v, 10)
  
  def apply(chars: String, radix: Int): Token = { //might be error token
    require(radix == 10 || radix == 16 || radix == 2 || radix == 8)
    val value: Option[BigInt] = try { (radix: @switch) match {
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
      
      case _ =>
        throw new IllegalArgumentException("impl error: invalid radix")
    } }
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

class FloatLit private(val chars: String, val value: BigDecimal) extends Token
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

class CharLit private(val value: Int, override val chars: String) extends Token
object CharLit {
  def apply(str: String): Token =
    try characterize(str) map { ch =>
      new CharLit(ch, "'" + str + "'")
    } getOrElse ErrorToken("Bad char literal: '" + str +"'")
    catch {
      case e: IllegalArgumentException =>
        ErrorToken("Invalid unicode code point in char literal '" + str + "'")
    }
  
  def unapply(tok: Token): Option[String] = tok match {
    case charLit: CharLit => Some(charLit.chars)
    case _ => None
  }
}

object StringLit {
  def raw(chars: String): StringLit =
    new StringLit(chars, "`" + chars + "`", true)
  
  def interp(chars: String): Token = {
    /*
      We use a list to avoid quadratic behavior on strings like
      "\n\n\n\n\n\n\n\n\n\n\n\n\n"
    */
    var ls = chars.toList
    val sb = new StringBuilder(chars.length)
    while (!ls.isEmpty) {
      //this is where quadratic behavior would occur
      val (pref, suf) = ls span { _ != '\\' }
      sb ++= pref
      if (!suf.isEmpty) {
        val (fancyEscape, afterEsc) = (suf(1): @switch) match {
          case 'x' | 'X' =>
            suf splitAt 4
          case 'u' =>
            suf splitAt 5
          case 'U' =>
            suf splitAt 9
          case ch =>
            if (ch.isDigit)
              suf splitAt 4
            else
              suf splitAt 2 // \n, etc
        }
        val escStr = fancyEscape.mkString
        characterize(escStr) match {
          case None =>
            return ErrorToken("Invalid escape sequence '" + escStr +
              "' in interpreted string literal \"" + chars + "\"")
          case Some(ch) =>
            sb append new String(Array(ch), 0, 1)
            ls = afterEsc
        }
      }
      else ls = Nil //or, ls = suf, which must be empty if we're here
    }
    new StringLit(sb.result, "\"" + chars + "\"", false)
  }
  
  def unapply(tok: Token): Option[String] = tok match {
    case stringLit: StringLit => Some(stringLit.value)
    case _ => None
  }
}
class StringLit private(val value: String, override val chars: String, val isRaw: Boolean) extends Token {
  override def toString = chars
}

object Token {
  private[lexer] def characterize(str: String): Option[Int] = {
    import java.lang.Long.parseLong
    
    if (str.length == 0)
      None
    else if (str(0) != '\\')
      if (str.length != 1) //length is the number of _code points_, not chars, in str.
        None
      else
        Some(str.codePointAt(0))
    else try {
      val result: Option[Int] = (str(1): @switch) match {
        case 'x' | 'X' =>
          if (str.length != 4) //as in: \xFF
            None
          else
            Some(parseLong(str.substring(2, 4), 16).toInt) //2 is the index of the char after x
        
        case 'u' =>
          if (str.length != 6) //as in: \uABCD
            None
          else
            Some(parseLong(str.substring(2, 6), 16).toInt) //2 is the index of the char after u
        
        case 'U' =>
          if (str.length != 10) //as in: \uAABBCCDD
            None
          else
            Some(parseLong(str.substring(2, 10), 16).toInt) //2 is the index of the char after U
        
        case 'a'  => if (str.length == 2) Some('\u0007') else None
        case 'b'  => if (str.length == 2) Some('\b')     else None
        case 'f'  => if (str.length == 2) Some('\f')     else None
        case 'n'  => if (str.length == 2) Some('\n')     else None
        case 'r'  => if (str.length == 2) Some('\r')     else None
        case 't'  => if (str.length == 2) Some('\t')     else None
        case 'v'  => if (str.length == 2) Some('\u000b') else None
        case '\\' => if (str.length == 2) Some('\\')     else None
        case '\'' => if (str.length == 2) Some('\'')     else None
        case '"'  => if (str.length == 2) Some('"')      else None //why not support '\"' if it makes my life easier?
              
        case ch =>
          if (ch.isDigit && str.length == 4) //as in: \377
            Some(parseLong(str.substring(1, 4), 8).toInt) //1 is the index of the char after \
          else
            None
      }
      result.filter(c => Character.isValidCodePoint(c) &&
                         !Character.isHighSurrogate(c.toChar) &&
                         !Character.isLowSurrogate(c.toChar))
    } catch {
      case e: NumberFormatException =>
        //println("Uh-oh.  A NumberFormatException for characterize('" + str + "')")
        None
    }
  }
}
