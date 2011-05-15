package jgo.compiler
package parser
package funcs

import scope._

import scoped._

import interm._
import instr._
import codeseq._
import symbol._

import scala.collection.mutable.{HashMap, HashSet}
import scala.{collection => coll}

trait FuncContext extends Base with StackScoped {
  protected def initialEnclosing: Scope
  //def target: Func
  //def compile: M[FuncData]
  
  private val seenLbls, unseenLbls = HashSet[String]()
  private val lbls = HashMap[String, UserLabel]()
  private val breakTargets, continueTargets = HashMap[String, Label]()
  
  def declLabel(lbl: String): UserLabel = {
    seenLbls += lbl
    unseenLbls -= lbl
    lbls getOrElseUpdate (lbl, new UserLabel(lbl))
  }
  
  def useLabel(lbl: String): UserLabel = {
    if (!(seenLbls contains lbl))
      unseenLbls += lbl
    lbls getOrElseUpdate (lbl, new UserLabel(lbl))
  }
  
  def breakTarget(lbl: String): Label = {
    if (!(seenLbls contains lbl))
      unseenLbls += lbl
    breakTargets getOrElseUpdate (lbl, new UserLabel(lbl))
  }
  
  def continueTarget(lbl: String): Label = {
    if (!(seenLbls contains lbl))
      unseenLbls += lbl
    continueTargets getOrElseUpdate (lbl, new UserLabel(lbl))
  }
  
  def declBreakable(lbl: String): Label = {
    seenLbls += lbl
    unseenLbls -= lbl
    breakTargets getOrElseUpdate (lbl, new UserLabel(lbl))
  }
  
  def declLoop(lbl: String): (Label, Label) = {
    seenLbls += lbl
    unseenLbls -= lbl
    (breakTargets    getOrElseUpdate (lbl, new UserLabel(lbl)),
     continueTargets getOrElseUpdate (lbl, new UserLabel(lbl + "#")))
  }
  
  def undecledLbls: coll.Set[String] = unseenLbls
}
