package jgo.compiler
package parser

import interm._
import types._

trait Operands extends CompositeLiterals with FunctionLiterals {
  self: Expressions =>
  
//def symbols: Scope
//def lexical = symbols //the lexical scope of any lambda expression
                        //herein is the current scope
  
  lazy val operand: P_ =         "operand" $
    ( "(" ~ expression ~ ")"
    | qualifiedIdent
    | methodAsFunc
    | literal
    )
    
  lazy val literal: P_ =   "literal value" $
    ( intLit
    | floatLit
//  | imaginaryLit
    | charLit
    | stringLit
    | compositeLit //nonterminal
    | functionLit  //nonterminal
    )
}
