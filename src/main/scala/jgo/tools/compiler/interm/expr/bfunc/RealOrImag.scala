package jgo.tools.compiler
package interm
package expr
package bfunc

import combin.{TypeChecks, ConstTypeCheckOverrides, CompatConstChecks}

import types._
import instr._
import instr.TypeConversions._

import scope.{UniverseScope => Univ}

sealed abstract class RealOrImag(getPartInstr: Complex => Instr, val name: String)
  extends BuiltinRegularFunc
  with TypeChecks
  with ConstTypeCheckOverrides {
  
  protected def selectFrom(r: BigDecimal, i: BigDecimal): BigDecimal
  
  private def floatingCorresp(ct: ComplexType): Floating = ct match {
    case Complex64  => F32
    case Complex128 => F64
  }
  
  private def resultTypeCorresp(ct: ComplexType): Type = ct match {
    case Complex64  => Univ.float32
    case Complex128 => Univ.float64
  }
  
  def invoke(args: List[Expr])(pos: Pos) = args match {
    case List(e) => e match {
      //As far as I can tell, spec never actually mentions untyped complex consts. Oh well.
      case UntypedComplexConst(r, i) => result(UntypedFloatingConst(selectFrom(r, i)))
      case TypedComplexConst(r, i, Underlying(ct: ComplexType)) =>
        result(TypedFloatingConst(selectFrom(r, i), resultTypeCorresp(ct)))
      case _ =>
        for (ct <- complex(e, "argument of %s" format name)(pos))
        yield UnderlyingExpr(e.evalUnder |+| getPartInstr(ct), resultTypeCorresp(ct))
    }
    case _ => problem("%s takes exactly one argument; found %s", name, cardinal(args.length))(pos)
  }
}

object Real extends RealOrImag(RealPart, "real") { protected def selectFrom(r: BigDecimal, i: BigDecimal) = r }
object Imag extends RealOrImag(ImagPart, "imag") { protected def selectFrom(r: BigDecimal, i: BigDecimal) = i }
