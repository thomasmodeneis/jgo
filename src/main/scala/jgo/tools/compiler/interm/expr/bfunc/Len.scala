package jgo.tools.compiler
package interm
package expr
package bfunc

import types._
import instr._

import scope.UniverseScope.int

object Len extends BuiltinRegularFuncExpr {
  def name = "len"
  
  def invoke(args: List[Expr])(pos: Pos) = args match {
    case List(arg) => arg match {
      case HasType(ArrayType(len, _)) => result(TypedIntegralConst(len, int))
      case StringConst(str)           => result(TypedIntegralConst(str.length, int))
      case HasType(StringType)        => result(UnderlyingExpr(arg.evalUnder |+| StringLen, int))
      case HasType(SliceType(_))      => result(UnderlyingExpr(arg.evalUnder |+| SliceLen, int))
      case HasType(MapType(_, _))     => result(UnderlyingExpr(arg.evalUnder |+| MapLen, int))
      case HasType(AnyChanType(_))    => result(UnderlyingExpr(arg.evalUnder |+| ChanLen, int))
      //TODO: ptr to array.
      case _ => problem("invalid argument to len; has type %s", arg.typeOf)(pos)
    }
    
    case _ => problem("len takes exactly one argument; found %s", cardinal(args.length))(pos)
  }
}
