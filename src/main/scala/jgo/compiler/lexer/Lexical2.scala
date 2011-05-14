package jgo.compiler
package lexer

import scala.util.parsing._
import input._
import CharSequenceReader.EofCh
import combinator._
import scala.annotation.{tailrec, switch}

import java.lang.Long.parseLong
import java.lang.Double.parseDouble

//portions of this object taken from scala.util.parsing.combinator.lexical.{Lexical, StdLexical}
object Lexical2 {
  private type Input = Reader[Char]
  
  def token(prev: Option[Token], in0: Input): Pair[Token, Input] = {
    val (tok, in) = processPrefix(prev, in0)
    val inRest = in.rest
    if (tok isDefined) (tok.get, in)
    else (in.first: @switch) match {
      case '0' => (inRest.first: @switch) match {
        case 'x' | 'X' =>
          val (num, suffix) = spanHexDigit(inRest.rest)
          (IntLit("0x" + num.mkString, 16), suffix)
        
        //I add support for binary literals because I think they are extremely useful and helpful.
        //Plus, they're pretty easy to implement.
        case 'b' | 'B' =>
          val (num, suffix) = spanNeq(inRest.rest, '0', '1')
          (IntLit("0b" + num.mkString, 2), suffix)
        
        case _ => digitInitNumeric(in)
      }
      
      //char literal
      case '\'' =>
        val (contents, restStartingWithDelim) = (inRest.first: @switch) match {
          case '\\' =>
            val (escTail, postEsc) = spanNeq(inRest.rest.rest, '\'', '\n', EofCh) //skip char after \
            ("\\" + inRest.rest.first + escTail, postEsc)
          case _ =>
            span(inRest) { ch => ch != '\'' && ch != '\n' && ch != EofCh }
        }
        (restStartingWithDelim.first: @switch) match {
          case '\'' => (CharLit(contents), restStartingWithDelim.rest)
          case _ => (ErrorToken("unenclosed character literal"), restStartingWithDelim)
        }
      
      //string literal
      case '"' =>
        val (contents, restStartingWithDelim) = {
          var (qFirstPart, r) = spanNeq(inRest, '\\', '\n', '"', EofCh)
          val sb = new StringBuilder(qFirstPart)
          while (r.first == '\\') {
            val afterBackslash = r.rest.first
            val (qPart, r2) = spanNeq(inRest, '\\', '\n', '"', EofCh) //skip the char after \
            r = r2
            sb append '\\' append afterBackslash append qPart //make sure to include the character immediately after \ which was skipped in the span
          }
          (sb.result, r)
        }
        (restStartingWithDelim.first: @switch) match {
          case '"' => (StringLit.interp(contents), restStartingWithDelim.rest)
          case _ => (ErrorToken("unenclosed string literal"), restStartingWithDelim)
        }
      
      //raw string literal
      case '`' =>
        val (contents, restStartingWithBacktick) = spanNeq(inRest, '`', EofCh)
        if (restStartingWithBacktick.first == EofCh)
          (ErrorToken("EOF in raw string literal"), restStartingWithBacktick)
        else
          (StringLit.raw(contents), restStartingWithBacktick.rest)
      
      case EofCh => (EOF, in)
      
      //dot-initial float literal, ".", or "..."
      case '.' =>
        if (inRest.first.isDigit) dotInitFloatLit(in.rest)
        else opOrDelim(in)
      
      //other cases
      case ch =>
        if (ch.isLetter || ch == '_') identLike(in)
        else if (ch.isDigit) digitInitNumeric(in)
        else opOrDelim(in)
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
  
  def identLike(in: Reader[Char]): (Token, Reader[Char]) = {
    val idBuilder = new StringBuilder
    var rest = in
    while (rest.first.isLetter || rest.first == '_' || rest.first.isDigit) {
      idBuilder += rest.first
      rest = rest.rest
    }
    val idStr = idBuilder.result
    val id = if (reserved contains idStr) Keyword(idStr) else Identifier(idStr)
    (id, rest)
  }
  
  def opOrDelim(in: Reader[Char]): (Token, Reader[Char]) = {
    val (opt, rest) = opsAndDelims.matchingPrefixOf(in)
    opt match {
      case Some(op) => (Keyword(op), rest)
      case None => (ErrorToken("invalid sequence, init char: " + in.first), in.rest)
    }
  }
  
  //def parseLong(num: String, radix: Int): Long = java.lang.Long.parseLong(num, radix)
  //def parseDouble(num: String): Double = java.lang.Double.parseDouble(num)
  
  def digitInitNumeric(in: Reader[Char]): Pair[Token, Reader[Char]] = {
    val (digits, suf) = spanDigit(in)
    (suf.first: @switch) match {
      case '.' =>
        val (digits2, suf2) = spanDigit(suf.rest)
        val (possExp, rest) = floatExpPart(suf2)
        possExp match {
          case Right(expStr) =>
           (FloatLit(digits + '.' + digits2 + expStr), rest)
          case Left(errMsg) =>
            (ErrorToken(errMsg), rest)
        }
      
      case 'e' | 'E' => 
       val (exp, rest) = floatExpPart(suf.rest)
       (FloatLit(digits + exp), rest)
      
      case _ => (digits(0): @switch) match {
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
    val (decimals, suf) = spanDigit(in)
    if (suf.first.toLower == 'e') {
      val (exp, rest) = floatExpPart(suf)
      (FloatLit("." + decimals + exp), rest)
    }
    else
      (FloatLit("." + decimals), suf)
  }
  
  //if in is prefixed with an 'e', that exponent paired with tail,
  //but if exponent invalid, None paired with in
  //else "" paired with in
  def floatExpPart(in: Reader[Char]): Pair[Either[String, String], Reader[Char]] =
    if (in.first.toLower == 'e') (in.rest.first: @switch) match {
      case '+' =>
        val (ds, rest) = spanDigit(in.rest.rest)
        val exp = "e+" + ds
        (Right(exp), rest)
    
      case '-' =>
        val (ds, rest) = spanDigit(in.rest.rest)
        val exp: String = "e-" + ds
        (Right(exp), rest)
      
      case _ =>
        val (ds, rest) = spanDigit(in.rest)
        val exp: String = "e" + ds
        (Right(exp), rest)
    }
    else (Right(""), in)
  
  def processPrefix(prev: Option[Token], in: Reader[Char]): (Option[Token], Reader[Char]) = prev match {
    case None => (None, stripWhitespace(in))
    case Some(id:    Identifier) => procForSemi(in)
    case Some(int:   IntLit)     => procForSemi(in)
    case Some(float: FloatLit)   => procForSemi(in)
  //case Some(imag:  ImagLit)    => procForSemi(in)
    case Some(char:  CharLit)    => procForSemi(in)
    case Some(str:   StringLit)  => procForSemi(in)
    case Some(Keyword(
           "break" |
           "continue" |
           "fallthrough" |
           "return" |
           "++" |
           "--" |
           ")" |
           "]" |
           "}" ))                => procForSemi(in)
    case _ => (None, stripWhitespace(in))
  }
  
  //TODO: add logic to detect empty lines and not produce ';' there
  @tailrec
  final def procForSemi(in: Reader[Char]): (Option[Token], Reader[Char]) = {
    if (in.atEnd)
      (None, in) //(Some(Keyword(";")), in) //Apr 24, 2011:  I see no reason why ; before EOF. Change grammar.
    else in.first match {
      case ' ' | '\t' | '\r' => procForSemi(in.rest)
      case '\n'       => (Some(Keyword(";")), in.rest)
    //case '}'        => (Some(Keyword(";")), in) //this is now part of the grammar
    //case ')'        => (Some(Keyword(";")), in)
      case '/' =>
        if (in.rest.first == '/') (Some(Keyword(";")), clearToNewline(in.rest))
        else (None, in)
      case '/' => 
        if (in.rest.first == '*') procForSemi(clearToStarSlash(in.rest))
        else (None, in)
      case other => (None, in)
    }
  }
  
  @tailrec
  final def stripWhitespace(in: Reader[Char]): Reader[Char] =
    if (in.atEnd)
      in
    else if (in.first == ' ' || in.first == '\n' || in.first == '\t' || in.first == '\r')
      stripWhitespace(in.rest)
    else in.first match {
      case ' ' | '\t' | '\n' | '\r' => procForSemi(in.rest)
      case '/' =>
        if (in.rest.first == '/') stripWhitespace(clearToNewline(in.rest))
        else in
      case '*' =>
        if (in.rest.first == '*') stripWhitespace(clearToStarSlash(in.rest))
        else in
      case other => in
    }
  
  
  def clearToNewline(in: Reader[Char]): Reader[Char] =
    dropWhile(in) { ch => ch != '\n' && ch != '\r' } rest
      
  @tailrec
  final def clearToStarSlash(in: Reader[Char]): Reader[Char] = {
    val cur = dropWhile(in)(_ != '*').rest
    cur.first match {
      case '/' => cur
      case _   => clearToStarSlash(cur)
    }
  }
  
  private def span(r: Reader[Char])(p: Char => Boolean): (String, Input) = {
    val str = new StringBuilder
    var cur = r
    while (p(cur.first)) {
      str += cur.first
      cur = cur.rest
    }
    (str.result, cur)
  }
  
  private def spanDigit(in: Input): (String, Input) = {
    val str = new StringBuilder
    var cur = in
    while (true) (cur.first: @switch) match {
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        str += cur.first
        cur = cur.rest
      case _ => return (str.result, cur)
    }
    throw new AssertionError("impl error: unreachable code reached")
  }
  
  private def spanHexDigit(in: Input): (String, Input) = {
    val str = new StringBuilder
    var cur = in
    while (true) (cur.first: @switch) match {
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' |
           'a' | 'b' | 'c' | 'd' | 'e' | 'f' |
           'A' | 'B' | 'C' | 'D' | 'E' | 'F' =>
        str += cur.first
        cur = cur.rest
      case _ => return (str.result, cur)
    }
    throw new AssertionError("impl error: unreachable code reached")
  }
  
  private def spanNeq(in: Input, c1: Char): (String, Input) = {
    val str = new StringBuilder
    var cur = in
    var ch = cur.first
    while (ch != c1) {
      str += ch
      cur = cur.rest
      ch = cur.first
    }
    (str.result, cur)
  }
  
  private def spanNeq(in: Input, c1: Char, c2: Char): (String, Input) = {
    val str = new StringBuilder
    var cur = in
    var ch = cur.first
    while (ch != c1 && ch != c2) {
      str += ch
      cur = cur.rest
      ch = cur.first
    }
    (str.result, cur)
  }
  
  private def spanNeq(in: Input, c1: Char, c2: Char, c3: Char): (String, Input) = {
    val str = new StringBuilder
    var cur = in
    var ch = cur.first
    while (ch != c1 && ch != c2 && ch != c3) {
      str += ch
      cur = cur.rest
      ch = cur.first
    }
    (str.result, cur)
  }
  
  private def spanNeq(in: Input, c1: Char, c2: Char, c3: Char, c4: Char): (String, Input) = {
    val str = new StringBuilder
    var cur = in
    var ch = cur.first
    while (ch != c1 && ch != c2 && ch != c3 && ch != c4) {
      str += ch
      cur = cur.rest
      ch = cur.first
    }
    (str.result, cur)
  }
  
  private def spanNeq(in: Input, c1: Char, c2: Char, c3: Char, c4: Char, c5: Char): (String, Input) = {
    val str = new StringBuilder
    var cur = in
    var ch = cur.first
    while (ch != c1 && ch != c2 && ch != c3 && ch != c4 && ch != c5) {
      str += ch
      cur = cur.rest
      ch = cur.first
    }
    (str.result, cur)
  }
  
  private def dropWhile(r: Input)(p: Char => Boolean): Input = {
    var cur = r
    while (p(cur.first))
      cur = cur.rest
    cur
  }
}
