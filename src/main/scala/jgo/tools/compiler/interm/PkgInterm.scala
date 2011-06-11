package jgo.tools.compiler
package interm

import codeseq._
import instr._
import symbol._
import types._

/**
 * The intermediate representation of a certain package.
 */
case class PkgInterm(
  target: Package,
  globals: List[GlobalVar],
  initCode: Code,
  functions: Map[Function, FunctionInterm])