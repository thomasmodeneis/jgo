package jgo.tools.compiler
package parser.stmts

import parser.exprs._
import parser.scoped._
import parser.funcs._

import interm._
import types._
import symbol._
import codeseq._
import instr._

import scala.collection.mutable.{HashMap, HashSet, ListBuffer}
import scala.{collection => coll}
import coll.{immutable => imm}

/**
 * Provides a mutable-state-based implementation of the semantics of
 * (user-defined) labels and gotos within the context of a certain
 * function.
 */
trait Labels {
  private val seenDefs   = HashSet[String]()
  private val unseenDefs = HashMap[String, ListBuffer[Pos]]()
  private val lbls = HashMap[String, UserLabel]()
  
  def defLabel(name: String, pos: Pos): (String, Err[UserLabel]) =
    if (seenDefs contains name)
      (name, problem("label %s already defined", name)(pos))
    else {
      seenDefs += name
      unseenDefs -= name
      val label = lbls getOrElseUpdate (name, new UserLabel(name))
      (name, result(label))
    }
  
  def useLabel(pos: Pos, name: String): UserLabel = {
    if (!(seenDefs contains name))
      unseenDefs.getOrElseUpdate(name, new ListBuffer) += pos
    lbls getOrElseUpdate (name, new UserLabel(name))
  }
  
  def procGoto(pos: Pos, name: String): Err[CodeBuilder] = {
    result(Goto(useLabel(pos, name)))
  }
  
  def checkForUndefedLabels: Err[Unit] = {
    var issues: Err[Unit] = result(())
    for ((lblName, positions) <- unseenDefs; pos <- positions) {
      issues = issues then problem("target label not found: %s", lblName)(pos)
    }
    issues
  }
}
