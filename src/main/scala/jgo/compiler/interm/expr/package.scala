package jgo.compiler
package interm

import message._

import types._
import symbol._
import instr._
import instr.TypeConversions._
import codeseq._

import scala.util.parsing.input.Position

package object expr {
  private[expr] type M[+T] = Messaged[T]
  private[expr] type Pos   = Position
  
  private[expr] def ordinal(n: Int): String = {
    require(n >= 0)
    (n: @scala.annotation.switch) match {
      case  0 => "zeroth"
      case  1 => "first"
      case  2 => "second"
      case  3 => "third"
      case  4 => "fourth"
      case  5 => "fifth"
      case  6 => "sixth"
      case  7 => "seventh"
      case  8 => "eighth"
      case  9 => "ninth"
      case 10 => "tenth"
      case 11 => "eleventh"
      case 12 => "twelfth"
      case 13 => "thirteenth"
      case 14 => "fourteenth"
      case 15 => "fifteenth"
      case 16 => "sixteenth"
      case 17 => "seventeenth"
      case 18 => "eighteenth"
      case 19 => "nineteenth"
      case 20 => "twentieth"
      case  i => i.toString + "th"
    }
  }
  
  
  private[expr] def checkCall(callee: Expr, args: List[Expr])(implicit pos: Pos): M[Type] = callee match {
    case HasType(FuncType(_, List(res0, res1, _*), _)) => Problem("polyadic results not currently supported")
    case HasType(FuncType(params, results, true))      => Problem("variadic calls not yet supported")
    
    case HasType(FuncType(params, results, false)) =>
      if (params.length != args.length)
        Problem("number (%d) of arguments passed unequal to number (%d) required",
                args.length, params.length)
      else {
        for (((param, HasType(arg)), index) <- (params zip args).zipWithIndex) if (!(param <<= arg))
          return Problem("%s argument has type %s not assignable to corresponding parameter type %s",
                         ordinal(index + 1), arg, param)
        Result(results.headOption getOrElse UnitType)
      }
    
    case _ => Problem("callee has type %s; function type required", callee.t)
  }
  
  
}
