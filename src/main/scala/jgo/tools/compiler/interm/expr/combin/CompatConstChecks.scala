package jgo.tools.compiler
package interm
package expr
package combin

import types._
import instr._
import instr.TypeConversions._
import codeseq._

import PartialFunction.condOpt

/**
 * Provides extractors for pairs of constants of compatible type.
 */
trait CompatConstChecks {
  
  protected object CompatStringConsts {
    def unapply(pair: (Expr, Expr)): Option[(String, String, Option[Type])] = condOpt(pair) {
      case (TypedStringConst(v1, t1), TypedStringConst(v2, t2)) if t1 == t2 => (v1, v2, Some(t1))
      case (UntypedStringConst(v1),   TypedStringConst(v2, t))              => (v1, v2, Some(t))
      case (TypedStringConst(v1, t),  UntypedStringConst(v2))               => (v1, v2, Some(t))
      case (UntypedStringConst(v1),   UntypedStringConst(v2))               => (v1, v2, None)
    }
  }
  
  protected object CompatBoolConsts {
    def unapply(pair: (Expr, Expr)): Option[(Boolean, Boolean, Option[Type])] = condOpt(pair) {
      case (TypedBoolConst(v1, t1), TypedBoolConst(v2, t2)) if t1 == t2 => (v1, v2, Some(t1))
      case (UntypedBoolConst(v1),   TypedBoolConst(v2, t))              => (v1, v2, Some(t))
      case (TypedBoolConst(v1, t),  UntypedBoolConst(v2))               => (v1, v2, Some(t))
      case (UntypedBoolConst(v1),   UntypedBoolConst(v2))               => (v1, v2, None)
    }
  }
  
  protected object CompatNumericConsts {
    def unapply(pair: (Expr, Expr)): Option[(BigDecimal, BigDecimal, BigDecimal, BigDecimal, Option[Type])] = condOpt(pair) {
      case (TypedNumericConst(r1, i1, t1), TypedNumericConst(r2, i2, t2)) if t1 == t2 => (r1, i1, r2, i2, Some(t1))
      case (UntypedNumericConst(r1, i1),   TypedNumericConst(r2, i2, t))              => (r1, i1, r2, i2, Some(t))
      case (TypedNumericConst(r1, i1, t),  UntypedNumericConst(r2, i2))               => (r1, i1, r2, i2, Some(t))
      case (UntypedNumericConst(r1, i1),   UntypedNumericConst(r2, i2))               => (r1, i1, r2, i2, None)
    }
  }
  
  protected object CompatRealConsts {
    def unapply(pair: (Expr, Expr)): Option[(BigDecimal, BigDecimal, Option[Type])] = condOpt(pair) {
      case (TypedRealConst(v1, t1), TypedRealConst(v2, t2)) if t1 == t2 => (v1, v2, Some(t1))
      case (UntypedRealConst(v1),   TypedRealConst(v2, t))              => (v1, v2, Some(t))
      case (TypedRealConst(v1, t),  UntypedRealConst(v2))               => (v1, v2, Some(t))
      case (UntypedRealConst(v1),   UntypedRealConst(v2))               => (v1, v2, None)
    }
  }
  
  protected object CompatIntegralConsts {
    def unapply(pair: (Expr, Expr)): Option[(BigInt, BigInt, Option[Type])] = condOpt(pair) {
      case (TypedIntegralConst(v1, t1), TypedIntegralConst(v2, t2)) if t1 == t2 => (v1, v2, Some(t1))
      case (UntypedIntegralConst(v1),   TypedIntegralConst(v2, t))              => (v1, v2, Some(t))
      case (TypedIntegralConst(v1, t),  UntypedIntegralConst(v2))               => (v1, v2, Some(t))
      case (UntypedIntegralConst(v1),   UntypedIntegralConst(v2))               => (v1, v2, None)
    }
  }
}
