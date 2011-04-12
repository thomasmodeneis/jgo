package jgo.compiler
package interm
package codeseq

import instr._

import scala.collection.LinearSeqOptimized
import scala.collection.immutable._
import scala.collection.generic._

object Code {
  def apply(elems: Instr*) = {
    val cb = new CodeBuilder
    for (elem <- elems)
      cb += elem
    cb.result
  }
}

sealed abstract class Code extends LinearSeq[Instr]
                              with LinearSeqOptimized[Instr, Code]
                              with Equals {
  def ::: (instr: Instr): Code = new :::(instr, this)
  
  def copy: Code = this match {
    case i ::: is => i ::: is.copy
    case Empty    => Empty
  }
}

case class ::: (head: Instr, private[codeseq] var tl: Code) extends Code {
  def tail    = tl
  def isEmpty = false
}

case object Empty extends Code {
  def head    = throw new NoSuchElementException("head of empty code")
  def tail    = throw new NoSuchElementException("tail of empty code")
  def isEmpty = true
  
  override def equals(that: Any) = that match {
    case that1: Seq[_] => that1.isEmpty
    case _             => false
  }
}
