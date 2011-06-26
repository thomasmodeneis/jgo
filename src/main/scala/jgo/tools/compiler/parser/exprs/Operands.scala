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
    | builtinCall
    | literal
    | InPos ~ symbol  ^^ procSymbOperand //yes, this *must* be last, to prevent preemptive prefix-matching
    | failure("not an operand")
    )
  
  lazy val builtinCall: Rule[Expr] =         "builtin func call" $
    ( onlyBfuncSymbol ~ "(" ~ onlyGoType ~ success(result(Nil)) <~ ")" ^^ bfuncTypeInvoke
    | onlyBfuncSymbol ~ "(" ~ onlyGoType ~    ("," ~> exprList) <~ ")" ^^ bfuncTypeInvoke
    | onlyBfuncSymbol ~ "(" ~ expr0List <~ ")"                         ^^ bfuncInvoke
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
      case ConstSymbol(c)       => result(c)
      case v: Variable          => result(Combinators.fromVariable(v))
      case f: Function          => result(Combinators.fromFunction(f))
      case s => problem("invalid operand: not a variable, constant, or function: %s", s)(pos)
    }
  
  protected def bfuncTypeInvoke(bfunc: BuiltinFuncExpr, pos: Pos, tErr: Err[Type], esErr: Err[List[Expr]]) =
    (tErr, esErr) flatMap { case (t, es) =>
      bfunc match {
        case b: BuiltinTypeFuncExpr => b.typeInvoke(t, es)(pos)
        case _ => problem("built-in func %s does not admit type-invocation", bfunc.name)(pos)
      }
    }
  
  protected def bfuncInvoke(bfunc: BuiltinFuncExpr, pos: Pos, esErr: Err[List[Expr]]) =
    esErr flatMap { es =>
      bfunc match {
        case b: BuiltinRegularFuncExpr => b.invoke(es)(pos)
        case _ => problem("missing type argument in invocation of built-in func %s", bfunc.name)(pos)
      }
    }
}
