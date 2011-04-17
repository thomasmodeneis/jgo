package jgo.compiler
package parser

import interm._
import types._

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
    | varSymbol   ^^ VarLval //yes, this *must* be last, to prevent preemptive prefix-matching (I hope you/I remember what this means!)
    | funcSymbol  ^^ { FuncExpr(_) }
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
