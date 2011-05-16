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
  private val seenDecls   = HashSet[String]()
  private val unseenDecls = HashMap[String, ListBuffer[Pos]]()
  private val lbls = HashMap[String, UserLabel]()
  
  def procLabelDecl(pos: Pos, name: String): M[UserLabel] =
    if (seenDecls contains name)
      Problem("label %s already declared", name)(pos)
    else {
      seenDecls += name
      unseenDecls -= name
      Result(lbls getOrElseUpdate (name, new UserLabel(name)))
    }
  
  def procGoto(pos: Pos, name: String): UserLabel = {
    if (!(seenDecls contains name))
      unseenDecls(name) += pos
    lbls getOrElseUpdate (name, new UserLabel(name))
  }
  
  def checkForUndecledLabels: M[Unit] = {
    var issues: M[Unit] = Result(())
    for ((lblName, positions) <- unseenDecls; pos <- positions) {
      issues = issues then Problem("target not found: %s", lblName)(pos)
    }
    issues
  }
}
