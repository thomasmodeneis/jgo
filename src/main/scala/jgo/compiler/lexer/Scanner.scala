package jgo.compiler
package lexer

import Lexical.token

import scala.util.parsing._
import input._
import combinator._

//portions of this class taken from scala.util.parsing.combinator.lexical.Scanners#Scanner
final class Scanner private(prev: Option[Token], in: Reader[Char]) extends Reader[Token] {
  private def this(in: Reader[Char]) = this(None, in)
  
  private val (tok, remainingIn) = token(prev, in)
  
  def      first = { /*println("      " + tok + " at " + pos);*/ tok }
  lazy val rest  = new Scanner(Some(tok), remainingIn)
  def      pos   = remainingIn.pos
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

object Scanner {
  import java.io.{File, InputStream, FileInputStream, InputStreamReader}
  import scala.collection.immutable.PagedSeq
  
  def apply(in: Reader[Char]): Scanner = new Scanner(None, in)
  def apply(inStr: String):    Scanner = new Scanner(new CharArrayReader(inStr.toCharArray()))
  def apply(in: File):         Scanner = apply(new FileInputStream(in))
  def apply(in: InputStream):  Scanner =
    new Scanner(None, new PagedSeqReader(PagedSeq.fromReader(new InputStreamReader(in , "UTF-8"))))
  
  def from(fileName: String): Scanner = apply(new FileInputStream(fileName))
}
