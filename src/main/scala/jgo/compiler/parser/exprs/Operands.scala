package jgo.compiler
package parser
package exprs

import interm._
import types._
import symbol._
import expr._
import expr.{Combinators => C}

trait Operands extends CompositeLiterals with ExprUtils /*with FunctionLiterals*/ {
  self: Expressions =>
  
//def symbols: Scope
//def lexical = symbols //the lexical scope of any lambda expression
                        //herein is the current scope
  
  //in general, "E = E ~ t2 | t1" MUST be used instead of "E = t1 | E ~ t2"
  lazy val operand: PM[Expr] =    "operand" $
    ( "(" ~> expression <~ ")"
//  | methodAsFunc
    | literal
    | InPos ~ symbol  ^^ procSymbOperand //yes, this *must* be last, to prevent preemptive prefix-matching
    | failure("not an operand")
    )
  
  lazy val literal: PM[Expr] =   "literal value" $
    ( intLit       ^^ { _.value } ^^ IntConstant
    | floatLit     ^^ { _.value } ^^ FloatConstant
//  | imaginaryLit
//  | charLit
    | stringLit    ^^ StringConst.apply
//  | compositeLit //nonterminal
//  | functionLit  //nonterminal
    )
  
  protected def procSymbOperand(pos: Pos, symbM: M[Symbol]): M[Expr] =
    symbM flatMap {
      case ConstSymbol(c) => Result(c)
      case v: Variable    => Result(varLval(v))
      case f: Function    => Result(functionExpr(f))
      case s => Problem("invalid operand: not a variable, constant, or function: %s", s)(pos)
    }
}
