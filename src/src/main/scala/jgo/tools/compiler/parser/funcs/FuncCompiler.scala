package jgo.tools.compiler
package parser
package funcs

import scope._

import scoped._

import interm._
import instr._
import codeseq._
import symbol._

/**
 * An object that compiles a certain func to its intermediate representation,
 * providing a func-context for the enclosed statements.
 */
abstract class FuncCompiler extends FuncContext {
  def target: Func
  def hasNamedResults: Boolean
  
  def compile: Err[FuncInterm]
  
  def targetFuncType = target.typeOf
}
