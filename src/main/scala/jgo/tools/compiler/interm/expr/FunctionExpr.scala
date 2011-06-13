package jgo.tools.compiler
package interm
package expr

import types._
import symbol._
import instr._
import instr.TypeConversions._
import codeseq._

/**
 * An expression referring to a function declared in package scope.
 * For example, in the expression `fmt.Println("hello, world")`,
 * the subexpression `fmt.Println`, which refers to the top-level
 * function `Println` declared in package `fmt`, would be represented
 * by an instance of FunctionExpr.
 *
 * @param f  a Function instance encapsulating information about the
 *           function this expression refers to which is needed for
 *           linking.
 */
private case class FunctionExpr(f: Function) extends Expr {
  val typeOf: FuncType = f.typeOf
  
  def eval = Function2Lambda(f)
  def evalUnder = eval
}
