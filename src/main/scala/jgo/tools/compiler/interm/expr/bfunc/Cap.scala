package jgo.tools.compiler
package interm
package expr
package bfunc

import types._
import instr._

import scope.UniverseScope.int

object Cap extends BuiltinRegularFuncExpr {
  def name = "cap"
  
  def invoke(args: List[Expr])(pos: Pos) = args match {
    case List(arg) => arg match {
      case HasType(ArrayType(len, _)) => result(TypedIntegralConst(len, int))
      case HasType(SliceType(_))      => result(UnderlyingExpr(arg.evalUnder |+| SliceCap, int))
      case HasType(AnyChanType(_))    => result(UnderlyingExpr(arg.evalUnder |+| ChanCap, int))
      //TODO: ptr to array.
      case _ => problem("invalid argument to cap, type %s", arg.typeOf)(pos)
    }
    
    case _ => problem("cap takes exactly one argument; found %s", cardinal(args.length))(pos)
  }
}
