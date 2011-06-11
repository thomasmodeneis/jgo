package jgo.tools.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

private object Utils {
  def checkCall(callee: Expr, args: List[Expr])(implicit pos: Pos): Err[Type] = callee match {
    case HasType(FuncType(_, List(res0, res1, _*), _)) => problem("polyadic results not currently supported")
    case HasType(FuncType(params, results, true))      => problem("variadic calls not yet supported")
    
    case HasType(FuncType(params, results, false)) =>
      if (params.length != args.length)
        problem("number (%d) of arguments passed unequal to number (%d) required",
                args.length, params.length)
      else {
        for (((param, HasType(arg)), index) <- (params zip args).zipWithIndex) if (!(param <<= arg))
          return problem("%s argument has type %s not assignable to corresponding parameter type %s",
                         ordinal(index + 1), arg, param)
        result(results.headOption getOrElse UnitType)
      }
    
    case _ => problem("callee has type %s; function type required", callee.typeOf)
  }
}
