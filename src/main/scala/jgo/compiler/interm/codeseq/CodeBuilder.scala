package jgo.compiler
package interm
package codeseq

import util._
import instr._

import scala.{collection => coll}
import coll.{mutable => mut}
import coll.{generic => gen}

object CodeBuilder {
  def empty =
    new CodeBuilder
  
  def apply(elems: Instr*): CodeBuilder = {
    val cb = new CodeBuilder
    for (elem <- elems)
      cb += elem
    cb
  }
}

/**
 * A mutable builder of sequences of intermediate code which supports
 * concatenation with other such builders.
 */
class CodeBuilder extends mut.Builder[Instr, Code] with Expendable {
  private[codeseq] def this(fst: Code, lst: Code) = {
    this()
    first = fst
    last  = lst
  }
  
  private var first: Code = CodeNil
  private var last:  Code = CodeNil
  
  /*
  private var resetReqd = false
  private def reset() {
    resetReqd = false
    if (isEmpty)
      return
    
    def copy(cur: Code): Code = cur match {
      case i ::: Empty =>
        last = i ::: Empty //this is a different instance
        last
      case i ::: is =>
        i ::: copy(is)
    }
    first = copy(first)
  }
  */
  
  override def toString = first.toString
  def listing = first.listing
  
  def isEmpty: Boolean = {
    errIfExpended()
    (first eq CodeNil) && (last eq CodeNil)
  }
  
  def clear() {
    errIfExpended()
    first = CodeNil
    last  = CodeNil
    //resetReqd = false
  }
  
  def += (instr: Instr): this.type = {
    errIfExpended()
    
    val add = instr ::: CodeNil
    last match {
      case CodeNil =>
        first = add
      case lst: (:::) =>
        lst.tl = add
    }
    last = add
    this
  }
  
  def result(): Code = {
    //resetReqd = true
    expend()
    first
  }
  
  def catZero = CodeBuilder.empty
  
  def |+| (other: CodeBuilder): CodeBuilder = {
    errIfExpended()
    if (other isEmpty)
      this
    else if (this isEmpty)
      other
    else last match {
      case CodeNil =>
        other
      case lst: (:::) =>
        lst.tl = other.first
        last   = other.last
        other.expend()   //no longer safe for other to be used
        this
    }
  }
  
  def |+| (instr: Instr): CodeBuilder =
    this += instr
}
