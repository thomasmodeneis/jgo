package jgo.compiler
package parser
package exprs

import interm._
import interm.types._

trait Operands extends CompositeLiterals /*with FunctionLiterals*/ {
  self: Expressions =>
  
//def symbols: Scope
//def lexical = symbols //the lexical scope of any lambda expression
                        //herein is the current scope
  
  lazy val operand: P[Expr] =    "operand" $
    ( "(" ~> expression <~ ")"
//  | qualifiedIdent
//  | methodAsFunc
//  | literal
    | onlyVarSymbol   ^^ VarLval //yes, this *must* be last, to prevent preemptive prefix-matching (I hope you/I remember what this means!)
    | onlyFuncSymbol  ^^ FuncExpr
    )
  
  /*
  lazy val literal: P[Expr] =   "literal value" $
    ( intLit
    | floatLit
//  | imaginaryLit
    | charLit
    | stringLit
//  | compositeLit //nonterminal
//  | functionLit  //nonterminal
    )
  */
}
