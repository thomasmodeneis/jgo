package jgo.compiler
package interm
package codeseq

import util._
import instr._

import scala.{collection => coll}
import coll.{mutable => mut}
import coll.{generic => gen}

object CodeBuilder {
  val empty = new CodeBuilder
  def apply(elems: Instr*): CodeBuilder = {
    val cb = new CodeBuilder
    for (elem <- elems)
      cb += elem
    cb
  }
  implicit def fromInstr(instr: Instr): CodeBuilder = {
    val ls = Code(instr)
    new CodeBuilder(ls, ls)
  }
}

class CodeBuilder extends mut.Builder[Instr, Code] with Expendable {
  private def this(fst: Code, lst: Code) = {
    this()
    first = fst
    last  = lst
  }
  
  private var first: Code = Empty
  private var last:  Code = Empty
  
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
  
  def isEmpty: Boolean = {
    errIfExpended()
    first == Empty && last == Empty
  }
  
  def clear() {
    errIfExpended()
    first = Empty
    last = Empty
    //resetReqd = false
  }
  
  def += (instr: Instr): this.type = {
    errIfExpended()
    
    val add = instr ::: Empty
    last match {
      case Empty =>
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
    else last match {
      case Empty =>
        other
      case lst: (:::) =>
        lst.tl = other.first
        last   = other.last
        other.expend()
        this
    }
  }
  
  def |+| (instr: Instr): CodeBuilder =
    this += instr
}
