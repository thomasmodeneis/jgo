package jgo.tools.compiler
package interm
package expr
package bfunc

import types._
import instr._

object Make extends BuiltinTypeFuncExpr {
  def name = "make"
  
  def typeInvoke(t: Type, args: List[Expr])(pos: Pos) = t.underlying match {
    case SliceType(et) => args match {
      case List() => problem("not enough arguments for slice-make; length unspecified")(pos)
      case List(len)      => result(UnderlyingExpr(len.evalUnder |+| MakeSliceLen(et), t))
      case List(len, cap) => result(UnderlyingExpr(len.evalUnder |+| cap.evalUnder |+| MakeSliceLenCap(et), t))
      case _      => problem("too many arguments for slice-make")(pos)
    }
    
    case MapType(k, v) => args match {
      case List()     => result(UnderlyingExpr(MakeMap(k, v), t))
      case List(size) => result(UnderlyingExpr(size.evalUnder |+| MakeMapSize(k, v), t))
      case _ => problem("too many arguments for map-make")(pos)
    }
    
    case AnyChanType(et) => args match {
      case List()     => result(UnderlyingExpr(MakeChan(et), t))
      case List(size) => result(UnderlyingExpr(size.evalUnder |+| MakeChanSize(et), t))
      case _ => problem("too many arguments for chan-make")(pos)
    }
    
    case _ => problem("cannot make type %s; slice, map, or chan type required", t)(pos)
  }
}
