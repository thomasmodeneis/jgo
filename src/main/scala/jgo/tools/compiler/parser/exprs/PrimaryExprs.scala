package jgo.tools.compiler
package parser.exprs

import parser.types._
import parser.scoped._

import interm._
import expr._
import expr.Combinators._
import types._
import codeseq._

trait PrimaryExprs extends Operands with Types {
  self: Expressions =>
  
  lazy val primaryExpr: LrRule[Expr] =                          "primary expression" $
    ( primaryExpr ~ "[" ~ expression <~ "]"                            ^^ index
    | primaryExpr ~ "[" ~ (expression.? <~ ":") ~ expression.? <~ "]"  ^^ slice
    | primaryExpr ~ "." ~ ident                                        ^^ select
    | primaryExpr ~ "." ~ ("(" ~> goType <~ ")")                       ^^ typeAssert
//  | onlyGoType ~ "(" ~ expression <~ ")"
    | primaryExpr ~ "(" ~ expr0List <~ ")"                             ^^ invoke
    | operand
    | failure("not a primary expression")
    )
}
