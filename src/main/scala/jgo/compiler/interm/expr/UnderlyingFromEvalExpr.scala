package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

/**
 * An expression whose eval-underlying code is to be computed
 * from its evaluation code using `Unwrap` instructions.
 */
private trait UnderlyingFromEvalExpr extends Expr {
  private[expr] def evalUnder: CodeBuilder = {
    var t = typeOf
    var code = eval
    while (true) t match {
      case wt: WrappedType =>
        code = code |+| Unwrap(wt)
        t = wt.referent
      case ta: TypeAlias =>
        t = ta.effective //or ta.referent
      case _ =>
        return code
    }
    throw new AssertionError("this expression should not be reached.")
  }
}
