package jgo.compiler
package parser
package funcs

import scope._

import scoped._

import interm._
import instr._
import codeseq._
import symbol._

/**
 * An object that compiles a certain func to its intermediate representation.
 */
abstract class FuncCompiler {
  def target:  M[Func]
  def compile: M[FuncInterm]
  def inAfter: Input
}
