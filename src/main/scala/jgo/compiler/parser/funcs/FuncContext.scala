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
  def target: Func
  def compile: M[FuncInterm]
}
