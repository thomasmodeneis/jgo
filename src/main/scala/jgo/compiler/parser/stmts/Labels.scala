package jgo.compiler
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

trait Labels {
  private val seenDefs   = HashSet[String]()
  private val unseenDefs = HashMap[String, ListBuffer[Pos]]()
  private val lbls = HashMap[String, UserLabel]()
  
  def defLabel(name: String, pos: Pos): (String, M[UserLabel]) =
    if (seenDefs contains name)
      (name, Problem("label %s already defined", name)(pos))
    else {
      seenDefs += name
      unseenDefs -= name
      val label = lbls getOrElseUpdate (name, new UserLabel(name))
      (name, Result(label))
    }
  
  def useLabel(pos: Pos, name: String): UserLabel = {
    if (!(seenDefs contains name))
      unseenDefs(name) += pos
    lbls getOrElseUpdate (name, new UserLabel(name))
  }
  
  def procGoto(pos: Pos, name: String): M[CodeBuilder] = {
    Result(Goto(useLabel(pos, name)))
  }
  
  def checkForUndefedLabels: M[Unit] = {
    var issues: M[Unit] = Result(())
    for ((lblName, positions) <- unseenDefs; pos <- positions) {
      issues = issues then Problem("target label not found: %s", lblName)(pos)
    }
    issues
  }
}
