package jgo.compiler
package lexer

import scala.util.parsing._
import input._
import CharSequenceReader.EofCh
import combinator._
import scala.annotation.tailrec

//portions of this class taken from scala.util.parsing.combinator.lexical.{Lexical, StdLexical}
class GoLexical extends GoTokenScanners {
  
  private implicit def extractor2Predicate(ex: CharExtractor): Char => Boolean =
    ex.unapply(_).isDefined
  private implicit def toExtractor(chr: Char): CharExtractor = new CharExtractor {
    def unapply(ch: Char): Option[Char] =
      if (ch == chr) Some(ch)
      else None
  }
  
  def token(prev: Option[Token], in0: Reader[Char]): Pair[Token, Reader[Char]] = {
    val (tok, in) = processPrefix(prev, in0)
    if (tok isDefined) (tok.get, in)
    else in match {
      case IdentLetter(_) ~: _ =>
        val (id, rest) = span(in)(IdentChar)
        (procIdentLike(id.mkString), rest)
      
      case '0' ~: post0 => post0 match {
        case x ~: hex if x.toLower == 'x' =>
          val (num, rest) = span(hex)(Range('0' -> '9', 'a' -> 'f', 'A' -> 'F'))
          (IntLit("0x" + num.mkString, 16), rest)
        
        case b ~: bin if b.toLower == 'b' =>
          val (num, rest) = span(bin)(Or('0', '1'))
          (IntLit("0b" + num.mkString, 2), rest)
        
        case _ => digitInitNumeric(in)
      }
      
      case Digit(_) ~: _ => digitInitNumeric(in)
      
      case '.' ~: Digit(_) ~: _ => dotInitFloatLit(in.rest)
      
      case '\'' ~: suf =>
        val (quote, restStartingWithDelim) = suf.first match {
          case '\\' =>
            val (escTail, rest) = span(suf.rest.rest) { ch => ch != '\'' && ch != '\n' && ch != EofCh } //skip char after \
            ("\\" + suf.rest.first + escTail, rest)
          case noEsc =>
            span(suf) { ch => ch != '\'' && ch != '\n' && ch != EofCh }
        }
        restStartingWithDelim.first match {
          case '\'' => (CharLit(quote), restStartingWithDelim.rest)
          case _ => (ErrorToken("Unenclosed character literal"), restStartingWithDelim)
        }
      
      case '"' ~: suf =>
        val (quote, restStartingWithDelim) = {
          var (qFirstPart, r) = span(suf) { ch => ch != '"' && ch != '\\' && ch != '\n' && ch != EofCh }
          val sb = new StringBuilder(qFirstPart)
          //println("the buffer is: " + sb)
          while (r.first == '\\') {
            val afterBackslash = r.rest.first
            val (qPart, r2) = span(r.rest.rest) { ch => ch != '"' && ch != '\\' && ch != '\n' && ch != EofCh } //skip the char after \
            r = r2
            //println("r.rest.first = " + afterBackslash + ", qPart = " + qPart)
            ((sb += '\\') += afterBackslash) ++= qPart //make sure to include the character immediately after \ which was skipped in the span
            //println("the buffer is: " + sb)
          }
          (sb.result, r)
        }
        restStartingWithDelim.first match {
          case '"' => (StringLit.interp(quote), restStartingWithDelim.rest)
          case _ => (ErrorToken("Unenclosed string literal"), restStartingWithDelim)
        }
    
//      case '"' ~: suf =>
//        val (quote, restStartingWithDelim) = {
//          var tuple = span(suf) { ch => ch != '"' && ch != '\\' && ch != '\n' && ch != EofCh }
//          while (tuple._2.first == '\\')
//            tuple = span(tuple._2.rest) { ch => ch != '"' && ch != '\\' && ch != '\n' && ch != EofCh }
//          tuple
//        }
//        restStartingWithDelim.first match {
//          case '"' => (StringLit.interp(quote), restStartingWithDelim.rest)
//          case _ => (ErrorToken("Unenclosed interpreted string literal"), restStartingWithDelim)
//        }
      
      case '`' ~: suf =>
        val (quote, restStartingWithBacktick) = span(suf){ch => ch != '`' && ch != EofCh}
        if (restStartingWithBacktick.first == EofCh)
          (ErrorToken("EOF in raw string literal"), restStartingWithBacktick)
        else
          (StringLit.raw(quote), restStartingWithBacktick.rest)
    
      case EofCh ~: _ => (EOF, in)
    
      case other =>
        val (opt, rest) = opsAndDelims.matchingPrefixOf(other)
        opt match {
          case Some(opOrDelim) => (Keyword(opOrDelim), rest)
          case None => (ErrorToken("Error: unexpected pattern, init char: " + in.first), in.rest)
        }
    }
  }
  
  val opsAndDelims = LexicalTrie("+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>", "&^", "+=",
                                 "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "&^=",
                                 "&&", "||", "<-", "++", "--", "==", "<", ">", "=", "!", "!=",
                                 "<=", ">=", ":=", "...", "(", ")", "[", "]", "{", "}", ",",
                                 ";", ".", ":")
  
  
  val reserved = Set("break", "case", "chan", "const", "continue", "default", "defer",
                     "else", "fallthrough", "for", "func", "go", "goto", "if", "import",
                     "interface", "map", "package", "range", "return", "select", "struct",
                     "switch", "type", "var")
  
  def procIdentLike(id: String): Token = 
    if (reserved contains id) Keyword(id) else Identifier(id)
  
  def parseLong(num: String, radix: Int): Long = java.lang.Long.parseLong(num, radix)
  def parseDouble(num: String): Double = java.lang.Double.parseDouble(num)
  
  def digitInitNumeric(in: Reader[Char]): Pair[Token, Reader[Char]] = {
    val (digits, suf) = span(in)(Digit)
    suf match {
      case '.' ~: suf2 =>
        val (digits2, suf3) = span(suf2)(Digit)
        val (possExp, rest) = floatExpPart(suf3)
        possExp match {
          case Right(expStr) =>
           (FloatLit(digits.mkString + '.' + digits2.mkString + expStr), rest)
          case Left(errMsg) =>
            (ErrorToken(errMsg), rest)
        }
      
      case exp if exp.first.toLower == 'e' => floatExpPart(exp) match {
        case (Right(exp1), rest) => (FloatLit(digits.mkString + exp1), rest)
        case (Left(err),  rest) => (ErrorToken(err), rest)
      }
      
      case _ => digits(0) match {
        case '0' =>
          //val (digits2, innerSuf) = digits span Range('0' -> '7')
          //val rest = (innerSuf.view.reverse :\ suf) ((_: Char) ~: (_: ReaderStream[Char]))
          //I should hope that innerSuf is empty; otherwise, we've recieved input like:
          //"const i = 077723983", which (though eventually invalid) must be lexed as:
          //Keyword("const"), Identifier("i"), Keyword("="),
          //IntLit(parseLong("077723", 8)), IntLit(983).
          //That is, we make sure to push "983" back onto the input by concat-ing it
          //to suf
          //eh.  I don't really care
          (IntLit(digits.mkString, 8), suf)
        case _ =>
          (IntLit(digits.mkString, 10), suf)
      }
    }
  }
  
  //in has the dot already stripped from it
  def dotInitFloatLit(in: Reader[Char]): Pair[Token, Reader[Char]] = {
    val (decimals, suf) = span(in)(Digit)
    if (suf.first.toLower == 'e') floatExpPart(suf) match {
      case (Right(exp), rest) => (FloatLit("." + decimals + exp), rest)
      case (Left(err),  rest) => (ErrorToken(err), rest)
    }
    else
      (FloatLit("." + decimals.mkString), suf)
  }
  
  //if in is prefixed with an 'e', that exponent paired with tail,
  //but if exponent invalid, None paired with in
  //else "" paired with in
  def floatExpPart(in: Reader[Char]): Pair[Either[String, String], Reader[Char]] =
    if (in.first.toLower == 'e') in.rest match {
      case '+' ~: Digit(d) ~: suf =>
        val (ds, rest) = span(suf)(Digit)
        val exp = "e+" + d + ds.mkString
        (Right(exp), rest)
    
      case '-' ~: Digit(d) ~: suf =>
        val (ds, rest) = span(suf)(Digit)
        val exp: String = "e-" + d + ds.mkString
        (Right(exp), rest)
      
      case Digit(d) ~: suf =>
        val (ds, rest) = span(suf)(Digit)
        val exp: String = "e" + d + ds.mkString
        (Right(exp), rest)
    
      case _ => (Left("Malformed floating-point literal exponent part"), in)
    }
    else (Right(""), in)
  
  def processPrefix(prev: Option[Token], in: Reader[Char]): (Option[Token], Reader[Char]) = {
    if (prev match {
          case Some(id:    Identifier) => true
          case Some(int:   IntLit)     => true
          case Some(float: FloatLit)   => true
        //case Some(imag:  ImagLit)    => true
          case Some(char:  CharLit)    => true
          case Some(str:   StringLit)  => true
          case Some(Keyword(k))
            if Set("break",
                   "continue",
                   "fallthrough",
                   "return",
                   "++", "--",
                   ")", "]", "}"
                  ) contains k    => true
          case _ => false
        })
      procForSemi(in)
    else
      (None, stripWhitespace(in))
  }
  
  //TODO: add logic to detect empty lines and not produce ';' there
  @tailrec
  final def procForSemi(in: Reader[Char]): (Option[Token], Reader[Char]) = {
    if (in.atEnd)
      (Some(Keyword(";")), in)
    else if (in.first == ' ' || in.first == '\t' || in.first == '\r')
      procForSemi(in.rest)
    else in match {
      case '\n' ~: tl        => (Some(Keyword(";")), tl)
    //case '}'  ~: tl        => (Some(Keyword(";")), in) //this is now part of the grammar
    //case ')'  ~: tl        => (Some(Keyword(";")), in)
      case '/'  ~: '/' ~: tl => (Some(Keyword(";")), clearToNewline(tl))
      case '/'  ~: '*' ~: tl => procForSemi(clearToStarSlash(tl))
      case other => (None, other)
    }
  }
  
  @tailrec
  final def stripWhitespace(in: Reader[Char]): Reader[Char] =
    if (in.atEnd)
      in
    else if (in.first == ' ' || in.first == '\n' || in.first == '\t' || in.first == '\r')
      stripWhitespace(in.rest)
    else in match {
      case '/' ~: '/' ~: tl => stripWhitespace(clearToNewline(tl))
      case '/' ~: '*' ~: tl => stripWhitespace(clearToStarSlash(tl))
      case other => other
    }
  
  
  def clearToNewline(in: Reader[Char]): Reader[Char] =
    dropWhile(in) {ch => ch != '\n' && ch != '\r'} rest
      
  @tailrec
  final def clearToStarSlash(in: Reader[Char]): Reader[Char] =
    dropWhile(in)(_ != '*').rest match {
      case '/' ~: tl => tl
      case _   ~: tl => clearToStarSlash(tl)
    }
  
  private def span(r: Reader[Char])(p: Char => Boolean): (String, Reader[Char]) = {
    val str = new StringBuilder
    var cur = r
    while (p(cur.first)) {
      str += cur.first
      cur = cur.rest
    }
    (str.result, cur)
  }
  private def dropWhile(r: Reader[Char])(p: Char => Boolean): Reader[Char] = {
    var cur = r
    while (p(cur.first))
      cur = cur.rest
    cur
  }
  
  private sealed abstract class CharExtractor {
    def unapply(ch: Char): Option[Char]
  }
  private object Or {
    def apply(a: CharExtractor, b: CharExtractor) = new CharExtractor {
      def unapply(ch: Char): Option[Char] = a unapply ch match {
        case Some(c) => Some(c)
        case None    => b unapply ch
      }
    }
    def apply(exts: CharExtractor*) = new CharExtractor {
      def unapply(ch: Char): Option[Char] = 
        if (exts exists {_(ch)}) Some(ch)
        else None
    }
  }
  private object Range {
    def apply(ls: Pair[Char, Char]*) = new CharExtractor {
      def unapply(ch: Char): Option[Char] = 
        if (ls exists {pair => pair._1 <= ch && ch <= pair._2})
          Some(ch)
        else
          None
    }
  }
  private object Not {
    def apply(a: CharExtractor) = new CharExtractor {
      def unapply(ch: Char): Option[Char] = a unapply ch match {
        case Some(_) => None
        case None    => Some(ch)
      }
    }
  }
  private object Letter extends CharExtractor {
    def unapply(ch: Char): Option[Char] =
      if (ch.isLetter) Some(ch)
      else None
  }
  private object IdentLetter extends CharExtractor {
    def unapply(ch: Char): Option[Char] =
      if (ch.isLetter || ch == '_') Some(ch)
      else None
  }
  private object Digit extends CharExtractor {
    def unapply(ch: Char): Option[Char] =
      if (ch.isDigit) Some(ch)
      else None
  }
  private object IdentChar extends CharExtractor {
    def unapply(ch: Char): Option[Char] =
      if (ch.isLetter || ch == '_' || ch.isDigit) Some(ch)
      else None
  }
}

object LexerTest {
  import java.io._
  import input._
  import scala.collection.immutable.PagedSeq
  
  def main(args: Array[String]) {
    object lex extends GoLexical
    var scan = if (args.length > 0) lex.Scanner(args(0))
               else lex.Scanner("/Volumes/KLAPERMAN/Horace Mann/Twelfth Grade/Computer Science/Old Go Compiler Code (Scala)/lexer_test_full.txt")
    
    while (!scan.atEnd) {
      println(scan.first)
      scan = scan.rest
    }
  }
}
