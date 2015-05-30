package jgo.tools.compiler
package lexer

import scala.util.parsing._
import input._
import CharSequenceReader.EofCh
import combinator._
import scala.annotation.{tailrec, switch}

import java.lang.Long.parseLong
import java.lang.Double.parseDouble

//portions of this object taken from scala.util.parsing.combinator.lexical.{Lexical, StdLexical}
object Lexical {
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
        val (contents, restStartingWithDelim) = inRest.first match {
          //escape sequence
          case '\\' =>
            //"inRest.rest.rest" -> skip char after \
            val (escTail, postEsc) = spanNeq(inRest.rest.rest, '\'', '\n', EofCh)
            ("\\" + inRest.rest.first + escTail, postEsc)
          
          case _ => spanNeq(inRest, '\'', '\n', EofCh)
        }
        restStartingWithDelim.first match {
          case '\'' => (CharLit(contents), restStartingWithDelim.rest)
          case _ => (ErrorToken("unenclosed character literal"), restStartingWithDelim)
        }
      
      //string literal
      case '"' =>
        val (contents, restStartingWithDelim) = {
          var (qFirstPart, r) = spanNeq(inRest, '"', '\\', '\n', EofCh)
          val sb = new StringBuilder(qFirstPart)
          while (r.first == '\\') {
            val afterBackslash = r.rest.first
            val (qPart, r2) = spanNeq(r.rest.rest, '\\', '\n', '"', EofCh) //skip the char after \
            r = r2
            sb ensureCapacity (sb.length + qPart.length + 2)
            //make sure to include the character immediately after \ which was skipped in the span
            (sb += '\\' += afterBackslash) append qPart
          }
          (sb.result, r)
        }
        restStartingWithDelim.first match {
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
  
  
  private def identLike(in: Input): (Token, Input) = {
    val idBuilder = new StringBuilder
    var cur = in
    while (cur.first.isLetter || cur.first == '_' || cur.first.isDigit) {
      idBuilder += cur.first
      cur = cur.rest
    }
    val idStr = idBuilder.result
    val id = if (reserved contains idStr) Keyword(idStr) else Identifier(idStr)
    (id, cur)
  }
  
  private def opOrDelim(in: Input): (Token, Input) = {
    val (opt, rest) = opsAndDelims.matchingPrefixOf(in)
    opt match {
      case Some(op) => (Keyword(op), rest)
      case None => (ErrorToken("invalid sequence, init char: " + in.first), in.rest)
    }
  }
  
  //in has the dot already stripped from it
  private def dotInitFloatLit(in: Input): (Token, Input) = {
    val (decimals, suf) = spanDigit(in)
    if (suf.first == 'e' || suf.first == 'E') {
      val (exp, rest) = floatExpPart(suf)
      (FloatLit("." + decimals + exp), rest)
    }
    else
      (FloatLit("." + decimals), suf)
  }
  
  private def digitInitNumeric(in: Input): (Token, Input) = {
    val (digits, suf) = spanDigit(in)
    (suf.first: @switch) match {
      //We've encountered a ".", so this must be a float lit
      case '.' =>
        val (digits2, suf2) = spanDigit(suf.rest)
        val (exp, rest) = floatExpPart(suf2)
        (FloatLit(digits + "." + digits2 + exp), rest)
      
      //We've encountered an exponent, so this must be a float lit
      case 'e' | 'E' => 
       val (exp, rest) = floatExpPart(suf)
       (FloatLit(digits + exp), rest)
      
      //We haven't encountered any of the above, so int lit
      case _ => digits(0) match {
        //Subtlety:  In the event of "0773999", digits will contain that entire string
        //The following code does NOT check for cases like that (which should be lexed,
        //in theory, as two separate int literals: the octal "0773" and the decimal "999")
        //since we know that they are ungrammatical anyway (the grammar doesn't permit
        //adjacent int literals).  So, we take the whole "0773999" as our "octal" literal
        //and let the IntLit injector cry error.
        case '0' => (IntLit(digits.mkString, 8), suf)  //octal
        case _   => (IntLit(digits.mkString, 10), suf) //decimal
      }
    }
  }
  
  //Check if this abides by the spec.
  //Right now, if in has a prefix like "e+abc", we return "e+",
  //followed by the remaining input starting with "abc".
  //When it comes time to implement imag literals, this function
  //will return a third value -- a boolean indicating whether
  //"i" was seen.
  private def floatExpPart(in: Input): (String, Input) =
    if (in.first == 'e' || in.first == 'E') (in.rest.first: @switch) match {
      case '+' =>
        val (ds, rest) = spanDigit(in.rest.rest)
        ("e+" + ds, rest)
    
      case '-' =>
        val (ds, rest) = spanDigit(in.rest.rest)
        ("e-" + ds, rest)
      
      case _ =>
        val (ds, rest) = spanDigit(in.rest)
        ("e" + ds, rest)
    }
    else ("", in)
  
  def processPrefix(prev: Option[Token], in: Input): (Option[Token], Input) = prev match {
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
  
  //TODO: add logic to detect empty lines and not produce ';' there //May 14, 2011: Necessary?
  @tailrec
  private def procForSemi(in: Input): (Option[Token], Input) = (in.first: @switch) match {
    case EofCh => (None, in) //(Some(Keyword(";")), in) //Apr 24, 2011:  I see no reason why ; before EOF. Change grammar.
    
    case ' ' | '\t' | '\r' => procForSemi(in.rest)
    
    case '\n' => (Some(Keyword(";")), in.rest)
    
    case '/' =>
      if (in.rest.first == '/') (Some(Keyword(";")), clearToLineEnd(in.rest.rest))
      else if (in.rest.first == '*') procForSemi(clearToStarSlash(in.rest.rest))
      else (None, in)
      
    case _ => (None, in)
  }
  
  @tailrec
  def stripWhitespace(in: Input): Input = (in.first: @switch) match {
    case EofCh => in
    case ' ' | '\t' | '\n' | '\r' => stripWhitespace(in.rest)
    case '/' =>
      if      (in.rest.first == '/') stripWhitespace(clearToLineEnd  (in.rest.rest))
      else if (in.rest.first == '*') stripWhitespace(clearToStarSlash(in.rest.rest))
      else in
    case _ => in
  }
  
  
  private def clearToLineEnd(in: Input): Input = {
    var cur = in
    //Note:  By mistake, I had initially written:
    //"cur != '\n' && cur != '\r' && cur != EofCh",
    //which, of course, is always true.  This is a
    //rare example of an error that Java's type
    //system would catch, but Scala's misses.
    while (cur.first != '\n' && cur.first != '\r' && cur.first != EofCh)
      cur = cur.rest
    cur.rest //char after the newline
  }
      
  @tailrec
  private def clearToStarSlash(in: Input): Input = {
    val cur = {
      var cur1 = in
      while (cur1.first != '*')
        cur1 = cur1.rest
      cur1.rest //char right after the next * encountered
    }
    cur.first match {
      case '/' => cur.rest
      case _   => clearToStarSlash(cur)
    }
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
}
