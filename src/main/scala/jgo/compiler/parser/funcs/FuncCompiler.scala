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

/**
 * An object that compiles a certain func to its intermediate representation.
 */
abstract class FuncCompiler extends Base with StackScoped {
  protected def initialEnclosing: Scope
  def target: Func
  def compile: M[FuncInterm]
  def hasNamedResults: Boolean
  
  def targetFuncType = target.t
  def resultTypes = targetFuncType.results
}
