package jgo.compiler
package parser.stmts

import parser.Base

import message._

import interm._
import types._
import symbol._
import codeseq._
import instr._

import scala.collection.mutable.{HashSet, HashMap}

trait BreaksAndContinues {
  self: Base =>
  
  private var breakStack, continueStack = List[(Option[String], Label)]()
  private val validBreaks, validContinues = HashMap[String, Label]()
  
  def breakable[T](p: Parser[M[Label => T]]): Parser[M[T]] = Parser { in =>
    val break = new Label("break")
    
    pushBreakable(break)
    val res = p(in) map { fM => fM(break) }
    popBreakable()
    res
  }
  
  def loop[T](p: Parser[M[(Label, Label) => T]]): Parser[M[T]] = Parser { in =>
    val g = new LabelGroup
    val break    = new Label("break", g)
    val continue = new Label("continue", g)
    
    pushLoop(break, continue)
    val res = p(in) map { fM => fM(break, continue) }
    popLoop()
    res
  }
  
  def breakable[T](nameP: Parser[M[String]], p: Parser[M[Label => T]]): Parser[M[T]] = Parser { in0 =>
    p(in0) flatMapWithNext { nameM => in =>
      val name = nameM getOrElse "<error>" //:(
      
      val break = new Label("(" + name + ").break")
      
      pushBreakable(break)
      val resParseResult = p(in) map { fM => fM(break) }
      popBreakable()
      resParseResult map { nameM then _ }
    }
  }
  
  def loop[T](nameP: Parser[M[String]], p: Parser[M[(Label, Label) => T]]): Parser[M[T]] = Parser { in0 =>
    p(in0) flatMapWithNext { nameM => in =>
      val name = nameM getOrElse "<error>" //:(
      
      val g = new LabelGroup
      val break    = new Label("(" + name + ").break", g)
      val continue = new Label("(" + name + ").continue", g)
      
      pushLoop(break, continue)
      val resParseResult = p(in) map { fM => fM(break, continue) }
      popLoop()
      resParseResult map { nameM then _ }
    }
  }
  
  def procBreak(pos: Pos) = breakStack.headOption match {
    case Some((_, target)) => Result(target)
    case None => Problem("illegal break; not enclosed by loop, switch, or select")(pos)
  }
  
  def procContinue(pos: Pos) = continueStack.headOption match {
    case Some((_, target)) => Result(target)
    case None => Problem("illegal continue; not enclosed by loop")(pos)
  }
  
  def procBreak(pos: Pos, name: String) = validBreaks get name match {
    case Some(target) => Result(target)
    case None => Problem("illegal break target %s; not an enclosing loop, switch, or select", name)(pos)
  }
  
  def procContinue(pos: Pos, name: String) = validContinues get name match {
    case Some(target) => Result(target)
    case None => Problem("illegal continue target %s; not an enclosing loop", name)(pos)
  }
  
  
  
  private def pushBreakable(breakTarget: Label) {
    breakStack = (None, breakTarget) :: breakStack
  }
  
  private def pushBreakable(name: String, breakTarget: Label) {
    breakStack = (Some(name), breakTarget) :: breakStack
    validBreaks(name) = breakTarget
  }
  
  private def pushLoop(breakTarget: Label, continueTarget: Label) {
    breakStack    = (None, breakTarget)    :: breakStack
    continueStack = (None, continueTarget) :: continueStack
  }
  
  private def pushLoop(name: String, breakTarget: Label, continueTarget: Label) {
    breakStack    = (Some(name), breakTarget)    :: breakStack
    continueStack = (Some(name), continueTarget) :: continueStack
    
    validBreaks(name)    = breakTarget
    validContinues(name) = continueTarget
  }
  
  private def popBreakable() {
    val (nameOpt, _) = breakStack.head
    breakStack = breakStack.tail
    for (name <- nameOpt)
      validBreaks -= name
  }
  
  private def popLoop() {
    val (nameOpt,  _) = breakStack.head
    val (name2Opt, _) = continueStack.head
    
    breakStack    = breakStack.tail
    continueStack = continueStack.tail
    
    assume(nameOpt == name2Opt)
    
    for (name <- nameOpt) {
      validBreaks    -= name
      validContinues -= name
    }
  }
}
