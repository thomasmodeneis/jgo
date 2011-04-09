package jgo.compiler
package lexer

import scala.util.parsing._
import input._
import combinator._

//portions of this trait taken from scala.util.parsing.combinator.lexical.Scanners
trait GoTokenScanners extends GoTokens {
  
  def token(prev: Option[Token], in: Reader[Char]): (Token, Reader[Char])
//  def stripWhitespace(in: Reader[Char]): Reader[Char]
  
  object Scanner {
    import java.io.{File, InputStream, FileInputStream, InputStreamReader}
    import scala.collection.immutable.PagedSeq
    
//  def apply(str: String): Scanner     = new Scanner(new CharArrayReader(str.toCharArray()))
    def apply(in: Reader[Char]): Scanner = new Scanner(None, in)
    def apply(fileName: String): Scanner = apply(new FileInputStream(fileName))
    def apply(in: File): Scanner         = apply(new FileInputStream(in))
    def apply(in: InputStream): Scanner  =
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
  
//  object ReaderStream extends SeqFactory[ReaderStream]{
//    implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LinearSeq[A]] = new GenericCanBuildFrom[A]
//    def newBuilder[A]: Builder[A, LinearSeq[A]] = new mutable.ListBuffer
//  }
  
//  object ReaderStream {
//    import java.io.{File, InputStream, FileInputStream, InputStreamReader}
//    import scala.collection.immutable.PagedSeq
//    
//    def apply(fileName: String): ReaderStream[Char] = apply(new FileInputStream(fileName))
//    def apply(in: File): ReaderStream[Char]         = apply(new FileInputStream(in))
//    def apply(in: InputStream): ReaderStream[Char]  = new ReaderStream(
//      new PagedSeqReader(PagedSeq.fromReader(new InputStreamReader(in , "UTF-8")))
//    )
//  }
//  
//  class ReaderStream[+A](private val r: Reader[A])
//  extends Reader[A]
//  with scala.collection.immutable.LinearSeq[A]
//  /* with scala.collection.LinearSeqOptimized[A, ReaderStream[A]] */ {
//  readerSelf: Reader[A] =>
//    println("Making new ReaderStream: " + this)
//    override def first                  = r.first
//    override def rest: ReaderStream[A]  = new ReaderStream(r.rest)
//    override def pos                    = r.pos
//    override def atEnd                  = r.atEnd
//    override def source                 = r.source
//    override def offset                 = r.offset
//    
//    override def drop(n: Int): ReaderStream[A] = readerSelf.drop(n)
//    override def dropWhile(p: A => Boolean): ReaderStream[A] = {
//      var cur = this
//      while (p(cur.first))
//        cur = cur.rest
//      cur
//    }
//    override def span(p: A => Boolean): (scala.collection.immutable.LinearSeq[A], ReaderStream[A]) =
//      (takeWhile(p),dropWhile(p))
//    
//    final def apply(idx: Int): A = {
//      require(idx >= 0)
//      var cur = this
//      var i = idx
//      while(i > 0) {
//        cur = cur.rest
//        i -= 1
//      }
//      cur.first
//    }
//    
//    lazy val length: Int = if (isEmpty) 0 else rest.length + 1
//    
//    override def head    = first
//    override def tail    = rest
//    override def isEmpty = r.atEnd
//  }
  
  object ~: {
    def unapply[A](r: Reader[A]): Option[Pair[A, Reader[A]]] =
      Some(r.first, r.rest)
  }
  
//  implicit def reader2SReadertream[U](r: Reader[U]): ReaderStream[U] =
//    new ReaderStream(r)
}

//object GoLexer {
//  def apply(str: String) = new GoLexer(new CharArrayReader(str.toCharArray()))
//}
//
////portions of this class taken from scala.util.parsing.combinator.lexical.Scanners.Scanner
//class GoLexer private(in: Reader[Char]) extends Reader[this.Token] with GoTokens {
//
////  private val (tok, rest1, rest2) = whitespace(in) match {
////    case Success(_, in1) => 
////      token(in1) match {
////        case Success(tok, in2) => (tok, in1, in2)
////        case ns: NoSuccess => (errorToken(ns.msg), ns.next, skip(ns.next))
////      }
////    case ns: NoSuccess => (errorToken(ns.msg), ns.next, skip(ns.next))
////  }
//  
//  private def skip(in: Reader[Char]) = if (in.atEnd) in else in.rest
//    
//  override def source: java.lang.CharSequence = in.source
//  override def offset: Int = in.offset
//  def first = tok
//  def rest = new Scanner(rest2)
//  def pos = rest1.pos
//  def atEnd = in.atEnd || (whitespace(in) match { case Success(_, in1) => in1.atEnd case _ => false })
//}
