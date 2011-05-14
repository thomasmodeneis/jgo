package jgo.compiler
package lexer

import scala.util.parsing._
import input._
import combinator._

//portions of this class taken from scala.util.parsing.combinator.lexical.Scanners#Scanner
final class OldScanner private(prev: Option[Token], in: Reader[Char]) extends Reader[Token] {
  private def this(in: Reader[Char]) = this(None, in)
  
  private val (tok, remainingIn) = OldLexical.token(prev, in)
  
  def      first = { /*println("      " + tok + " at " + pos);*/ tok }
  lazy val rest  = new OldScanner(Some(tok), remainingIn)
  lazy val pos   = OldLexical.stripWhitespace(in).pos
  def      atEnd = tok == EOF
  
  override def source = in.source
  override def offset = in.offset
  
  def foreach[U](f: Token => U) {
    var cur = this
    while (!cur.atEnd) {
      f(cur.first)
      cur = cur.rest
    }
  }
}

object OldScanner {
  import java.io.{File, InputStream, FileInputStream, InputStreamReader}
  import scala.collection.immutable.PagedSeq
  
  def apply(in: Reader[Char]): OldScanner = new OldScanner(None, in)
  def apply(inStr: String):    OldScanner = new OldScanner(new CharArrayReader(inStr.toCharArray()))
  def apply(in: File):         OldScanner = apply(new FileInputStream(in))
  def apply(in: InputStream):  OldScanner =
    new OldScanner(None, new PagedSeqReader(PagedSeq.fromReader(new InputStreamReader(in , "UTF-8"))))
  
  def from(fileName: String): OldScanner = apply(new FileInputStream(fileName))
}
