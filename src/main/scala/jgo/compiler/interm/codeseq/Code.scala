package jgo.compiler
package interm
package codeseq

import instr._

import scala.collection.LinearSeqOptimized
import scala.collection.immutable._
import scala.collection.generic._
import scala.collection.mutable.Builder

object Code {
  def apply(elems: Instr*) = {
    val cb = new CodeBuilder
    for (elem <- elems)
      cb += elem
    cb.result
  }
}

sealed abstract class Code extends LinearSeq[Instr] with LinearSeqOptimized[Instr, Code] {
  override def newBuilder: Builder[Instr, Code] = new CodeBuilder
  
  def ::: (instr: Instr): Code = new :::(instr, this)
  
  def copy: Code = this match {
    case i ::: is => i ::: is.copy
    case CodeNil  => CodeNil
  }
  
  override def toString: String = {
    if (isEmpty)
      return "{ }"
    val sb = new StringBuilder("{ ")
    var cur = this
    while (!cur.tail.isEmpty) {
      sb append cur.head append "; "
      cur = cur.tail
    }
    sb append cur.head append " }"
    sb.result
  }
  
  def listing: String = {
    if (isEmpty)
      return ""
    val sb = new StringBuilder("\n")
    var first = true
    var prevJump, prevLbl, prevDecl, prevUndecl = false
    for (instr <- this) {
      var sawJump, sawLbl, sawDecl, sawUndecl = false
      instr match {
        case _: ControlFlow =>
          sb append instr append "\n\n"
          sawJump = true
        
        case _: Lbl =>
          if (!prevJump)
            sb append "\n"
          sb append instr append "\n"
          sawLbl = true
        
        case _: Decl =>
          if (!prevJump && !prevLbl && !prevDecl && !first)
            sb append "\n"
          sb append instr append "\n"
          sawDecl = true
        
        case _: Undecl =>
          sb append instr append "\n"
          sawUndecl = true
        
        case _ =>
          if (prevUndecl)
            sb append "\n"
          sb append instr append "\n"
      }
      first = false
      prevJump   = sawJump
      prevLbl    = sawLbl
      prevDecl   = sawDecl
      prevUndecl = sawUndecl
    }
    sb.result
  }
}

case class ::: (override val head: Instr, private[codeseq] var tl: Code) extends Code {
  override def tail    = tl
  override def isEmpty = false
}

case object CodeNil extends Code {
  override def head    = throw new NoSuchElementException("head of empty code")
  override def tail    = throw new NoSuchElementException("tail of empty code")
  override def isEmpty = true
  
  /*
  override def equals(that: Any) = that match {
    case that1: Code => that1.canEqual(this) && that1.isEmpty
    case _           => false
  }
  */
  
  override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
  
  override def canEqual(o: Any) = o == CodeNil
}
