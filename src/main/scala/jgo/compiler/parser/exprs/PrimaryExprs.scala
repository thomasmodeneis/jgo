package jgo.compiler
package parser.exprs

import message._

import parser.types._
import parser.scoped._

//import interm._
import interm.expr._
import interm.expr.{Combinators => C}
import interm.types._
import interm.codeseq._

trait PrimaryExprs extends Operands with TypeSyntax with Scoped with ExprUtils {
  self: Expressions =>
  
  lazy val primaryExpr: PPM[Expr] =                          "primary expression" $
    ( primaryExpr ~ pos("[") ~ expression <~ "]"                            ^^ C.index
    | primaryExpr ~ pos("[") ~ (expression.? <~ ":") ~ expression.? <~ "]"  ^^ C.slice
//  | primaryExpr ~ pos(".") ~ ident
    | primaryExpr ~ pos(".") ~ ("(" ~> goType <~ ")")                       ^^ C.typeAssert
    | primaryExpr ~ pos("(") ~ exprList <~ ")"                              ^^ C.invoke
//  | (goType <~ "(") ~ expression <~ ")"                     &@ "unambiguous type conversion"
//  | specialBuiltinTypeCall //not yet supported
    | operand
    )
}
