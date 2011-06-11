package jgo.tools.compiler
package parser
package exprs

import interm._
import types._
import symbol._
import expr._
import expr.Combinators

trait Operands extends CompositeLiterals with ExprUtils /*with FunctionLiterals*/ {
  self: Expressions =>
  
  //in general, "E = E ~ t2 | t1" MUST be used instead of "E = t1 | E ~ t2"
  lazy val operand: Rule[Expr] =                       "operand" $
    ( "(" ~> expression <~ ")"
//  | methodAsFunc
    | literal
    | InPos ~ symbol  ^^ procSymbOperand //yes, this *must* be last, to prevent preemptive prefix-matching
    | failure("not an operand")
    )
  
  lazy val literal: Rule[Expr] =                 "literal value" $
    ( intLit       ^^ { i => result(UntypedIntegralConst(i)) }
    | floatLit     ^^ { f => result(UntypedFloatingConst(f)) }
//  | imaginaryLit
    | charLit      ^^ { c => result(UntypedIntegralConst(c)) }
    | stringLit    ^^ { s => result(UntypedStringConst(s)) }
//  | compositeLit //nonterminal
//  | functionLit  //nonterminal
    )
  
  protected def procSymbOperand(pos: Pos, symbErr: Err[Symbol]): Err[Expr] =
    symbErr flatMap {
      case ConstSymbol(c) => result(c)
      case v: Variable    => result(Combinators.fromVariable(v))
      case f: Function    => result(Combinators.fromFunction(f))
      case s => problem("invalid operand: not a variable, constant, or function: %s", s)(pos)
    }
}
