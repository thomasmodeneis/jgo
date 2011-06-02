package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

/**
 * An expression whose evaluation code is be computed
 * from its eval-underlying code using `Wrap` instructions.
 */
private trait EvalFromUnderlyingExpr extends Expr {
  private[expr] def eval = {
    def evalWrappedIn(t: Type): CodeBuilder = t match {
      case wt: WrappedType => evalWrappedIn(wt.referent) |+| Wrap(wt)
      case ta: TypeAlias   => evalWrappedIn(ta.effective) //or ta.referent
      case _               => evalUnder
    }
    evalWrappedIn(typeOf)
  }
}
