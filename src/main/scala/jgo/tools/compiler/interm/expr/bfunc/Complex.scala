package jgo.tools.compiler
package interm
package expr
package bfunc

import combin.{TypeChecks, ConstTypeCheckOverrides, CompatConstChecks}

import types._
import instr._
import instr.TypeConversions._

import scope.{UniverseScope => Univ}

object Complex extends BuiltinRegularFuncExpr with CompatConstChecks with TypeChecks with ConstTypeCheckOverrides {
  def name = "complex"
  
  def invoke(args: List[Expr])(pos: Pos) = args match {
    case List(e1, e2) => (e1, e2) match {
      case CompatRealConsts(r, i, tOpt) => tOpt match {
        //according to spec, c128 in case of untyped, not untyped.
        case None | Some(Underlying(Float64)) => result(TypedComplexConst(r, i, Univ.complex128))
        case Some(Underlying(Float32))        => result(TypedComplexConst(r, i, Univ.complex64))
        case Some(t) => problem("illegal type %s in call to complex", t)(pos)
      }
      case _ =>
        for ((e1f, e2f, ft) <- sameFloating(e1, e2)(pos))
        yield {
          val (ct, resultType) = ft match {
            case Float32 => (Complex64,  Univ.complex64)
            case Float64 => (Complex128, Univ.complex128)
          }
          UnderlyingExpr(e1f.evalUnder |+| e2f.evalUnder |+| MakeComplex(ct), resultType)
        }
    }
    case _ => problem("complex takes exactly two arguments; found %s", cardinal(args.length))(pos)
  }
}
