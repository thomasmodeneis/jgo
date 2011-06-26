package jgo.tools.compiler
package interm
package expr
package bfunc

import types._
import instr._
import codeseq.CodeBuilder

object Make extends BuiltinTypeFuncExpr {
  def name = "make"
  
  private def intCode(e: Expr, desc: String)(pos: Pos): Err[CodeBuilder] = e match {
    case UntypedIntegralConst(i)   => result(PushInt(i, I32))
    case HasType(iT: IntegralType) => result(e.evalUnder)
    case _ => problem("specified %s not of integral type", desc)(pos)
    //"specified size not of integral type", etc.
  }
  
  def typeInvoke(t: Type, args: List[Expr])(pos: Pos) = t.underlying match {
    case SliceType(et) => args match {
      case List() => problem("not enough arguments for slice-make; length unspecified")(pos)
      case List(len) =>
        for (lenC <- intCode(len, "length")(pos))
        yield UnderlyingExpr(lenC |+| MakeSliceLen(et), t)
      case List(len, cap) =>
        for ((lenC, capC) <- (intCode(len, "length")(pos), intCode(cap, "capacity")(pos)))
        yield UnderlyingExpr(lenC |+| capC |+| MakeSliceLenCap(et), t)
      case _ => problem("too many arguments for slice-make")(pos)
    }
    
    case MapType(k, v) => args match {
      case List()     => result(UnderlyingExpr(MakeMap(k, v), t))
      case List(size) =>
        for (sizeC <- intCode(size, "size")(pos))
        yield UnderlyingExpr(sizeC |+| MakeMapSize(k, v), t)
      case _ => problem("too many arguments for map-make")(pos)
    }
    
    case AnyChanType(et) => args match {
      case List()     => result(UnderlyingExpr(MakeChan(et), t))
      case List(size) =>
        for (sizeC <- intCode(size, "size")(pos))
        yield UnderlyingExpr(sizeC |+| MakeChanSize(et), t)
      case _ => problem("too many arguments for chan-make")(pos)
    }
    
    case _ => problem("cannot make type %s; slice, map, or chan type required", t)(pos)
  }
}
