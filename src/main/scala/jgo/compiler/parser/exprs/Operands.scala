package jgo.compiler
package parser
package exprs

//import interm._
import interm.types._
import interm.expr._

trait Operands extends CompositeLiterals /*with FunctionLiterals*/ {
  self: Expressions =>
  
//def symbols: Scope
//def lexical = symbols //the lexical scope of any lambda expression
                        //herein is the current scope
  
  //in general, "E = E ~ t2 | t1" MUST be used instead of "E = t1 | E ~ t2"
  lazy val operand: PM[Expr] =    "operand" $
    ( "(" ~> expression <~ ")"
//  | qualifiedIdent
//  | methodAsFunc
//  | literal
//  | onlyVarSymbol   ^^ VarLval //yes, this *must* be last, to prevent preemptive prefix-matching (I hope you/I remember what this means!)
//  | onlyFuncSymbol  ^^ FuncExpr
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
