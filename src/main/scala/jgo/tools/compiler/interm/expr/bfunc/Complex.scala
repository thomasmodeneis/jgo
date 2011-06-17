package jgo.tools.compiler
package interm
package expr
package bfunc

import combin.{TypeChecks, ConstTypeCheckOverrides}

import types._
import instr._

import scope.UniverseScope.{complex64, complex128}

object Complex extends BuiltinRegularFuncExpr with TypeChecks with ConstTypeCheckOverrides {
  def name = "complex"
  
  def invoke(args: List[Expr])(pos: Pos) = args match {
    case List(e1, e2) => (e1, e2) match {
      case (UntypedRealConst(r), UntypedRealConst(i)) => result(UntypedComplexConst(r, i))
      case Untyped
    }
    
    case _ => problem("complex takes exactly two arguments; found %s", cardinal(args.length))(pos)
  }
}
