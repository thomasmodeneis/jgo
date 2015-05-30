package jgo.tools.compiler
package parser
package funcs

import scope._

import scoped._

import interm._
import interm.types._
import instr._
import codeseq._
import symbol._

trait FuncContext extends {
  def targetFuncType: FuncType
  def hasNamedResults: Boolean
  
  def resultTypes = targetFuncType.results
}