package jgo.compiler
package interm
package expr

import types._
import symbols._
import instr._
import instr.TypeConversions._
import codeseq._

trait Expr extends Typed {
  /**
   * Provides the code necessary for computing the value
   * of this expression and placing the result on the top
   * of the operand stack. This code is called the
   * <i>evaluation code</i> of this expression.
   */
  def eval: CodeBuilder
  
  private[expr] def addressable = false
    
  def call(args: List[Expr]): Either[String, Expr] =
    for (resultT <- Expr.checkCall(funcType, args).right)
    yield SimpleExpr((args foldLeft eval) { _ |+| _.eval } |+| InvokeLambda(funcType.get), resultT)
}

object Expr {
  def apply(eval: => CodeBuilder, t: Type)
  private def ordinal(n: Int): String = {
    require(n >= 0)
    (n: @scala.annotation.switch) match {
      case 0 => "zeroth"
      case 1 => "first"
      case 2 => "second"
      case 3 => "third"
      case 4 => "fourth"
      case 5 => "fifth"
      case 6 => "sixth"
      case 7 => "seventh"
      case 8 => "eighth"
      case 9 => "ninth"
      case i => i.toString + "th"
    }
  }
  
  private[expr] def checkCall(funcType: Option[FuncType], args: List[Expr]): Either[String, Type] = funcType match {
    case None => Left("not of function type")
    
    case Some(FuncType(_, List(res0, res1, _*), _)) => Left("polyadic results not currently supported")
    
    case Some(FuncType(params, results, false)) =>
      if (params.length != args.length)
        Left("number (%d) of arguments passed unequal to number (%d) required" format (args.length, params.length))
      else {
        for (((param, HasType(arg)), index) <- (params zip args).zipWithIndex)
          if (!(param <<= arg))
            return Left("%s argument has type %s not assignable to corresponding parameter type %s" format (Expr.ordinal(index + 1), arg, param))
        Right(results.headOption getOrElse UnitType)
      }
    
    case Some(FuncType(params, results, true)) => Left("variadic calls not yet supported")
  }
}
