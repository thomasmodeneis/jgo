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

sealed abstract class Code extends LinearSeqOptimized[Instr, Code] with Equals {
  def newBuilder: Builder[Instr, Code] = new CodeBuilder
  
  def ::: (instr: Instr): Code = new :::(instr, this)
  
  def copy: Code = this match {
    case i ::: is => i ::: is.copy
    case Empty    => Empty
  }
}

case class ::: (override val head: Instr, private[codeseq] var tl: Code) extends Code {
  override def tail    = tl
  override def isEmpty = false
}

case object Empty extends Code {
  override def head    = throw new NoSuchElementException("head of empty code")
  override def tail    = throw new NoSuchElementException("tail of empty code")
  override def isEmpty = true
  
  override def equals(that: Any) = that match {
    case that1: Seq[_] => that1.isEmpty
    case _             => false
  }
}
