package jgo.compiler
package lexer

import scala.util.parsing._
import input._
import combinator._

//portions of this trait taken from scala.util.parsing.combinator.lexical.Scanners
trait Scanners {
  //def token(prev: Option[Token], in: Reader[Char]): (Token, Reader[Char])
  import Lexical.token
  
  object Scanner {
    import java.io.{File, InputStream, FileInputStream, InputStreamReader}
    import scala.collection.immutable.PagedSeq
    
    def apply(in: Reader[Char]): Scanner = new Scanner(None, in)
    def apply(fileName: String): Scanner = apply(new FileInputStream(fileName))
    def apply(in: File):         Scanner = apply(new FileInputStream(in))
    def apply(in: InputStream):  Scanner =
      new Scanner(None, new PagedSeqReader(PagedSeq.fromReader(new InputStreamReader(in , "UTF-8"))))
  }
  
  class Scanner private(prev: Option[Token], in: Reader[Char]) extends Reader[Token] {
    private val (tok, remainingIn) = token(prev, in)
    
    def      first = { /*println("      " + tok + " at " + pos);*/ tok }
    lazy val rest  = new Scanner(Some(tok), remainingIn)
    def      pos   = remainingIn.pos
    def      atEnd = tok == EOF
    
    override def source = in.source
    override def offset = in.offset
  }
}
