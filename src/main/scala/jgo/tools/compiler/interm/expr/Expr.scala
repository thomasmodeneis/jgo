package jgo.tools.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

/**
 * An object that encapsulates information about an expression;
 * most significantly, the type of that expression and the code
 * which pushes its value onto the operand stack.
 */
trait Expr extends Typed {
  /**
   * Provides the code necessary for computing the value
   * of this expression and placing the result on the top
   * of the operand stack. This code is called the
   * ''evaluation code'' of this expression.
   */
  private[expr] def eval: CodeBuilder
  
  /**
   * Provides the code necessary for computing and stacking
   * the value of this expression as an instance of its
   * underlying type.  This code is called the ''eval-underlying
   * code'' of this expression.  We avoid the term "underlying
   * evaluation code" because it implies that this code
   * is a subsequence of the evaluation code, which is not always
   * the case.
   * 
   * The private trait `UnderlyingFromEvalExpr` provides an
   * implementation of this method that computes the
   * eval-underlying code from the evaluation code.
   */
  private[expr] def evalUnder: CodeBuilder
  
  /**
   * States whether this expression may be used as the operand
   * of the address-of operator.
   */
  private[expr] def addressable = false
  
  private[expr] def mkPtr: Expr = throw new UnsupportedOperationException(
    "JGo internal implementation error:  "
    + "calling mkPtr on an unsuitable Expr; "
    + "addressable = " + addressable
  )
  
  //Bad abstraction.  Improve.
  private[expr] def mkCall(args: List[Expr], resultT: Type): Expr =
    EvalExpr((args foldLeft evalUnder) { _ |+| _.eval } |+| InvokeLambda(Lambda(funcType.get)), resultT)
}
