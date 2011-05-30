package jgo.compiler
package parser.exprs

import parser.types._

import interm._
import expr._
import expr.Combinators._
import types._

trait Expressions extends PrimaryExprs with ExprUtils {
  lazy val expression: LrRule[Expr] =                       "expression" $
    catchSyntaxErr(orExpr, "not an expression")
  
  lazy val orExpr: LrRule[Expr] =                "or-expression: prec 1" $
    ( orExpr ~ "||" ~ andExpr   ^^ or
    | andExpr
    )
  
  lazy val andExpr: LrRule[Expr] =              "and-expression: prec 2" $
    ( andExpr ~ "&&" ~ relExpr  ^^ and
    | relExpr
    )
  
  lazy val relExpr: LrRule[Expr] =       "relational expression: prec 3" $
    ( relExpr ~ "==" ~ addExpr  ^^ compEq
    | relExpr ~ "!=" ~ addExpr  ^^ compNe
    | relExpr ~ "<"  ~ addExpr  ^^ compLt
    | relExpr ~ "<=" ~ addExpr  ^^ compLeq
    | relExpr ~ ">"  ~ addExpr  ^^ compGt
    | relExpr ~ ">=" ~ addExpr  ^^ compGeq
    | addExpr
    )
  
  lazy val addExpr: LrRule[Expr] =         "additive expression: prec 4" $
    ( addExpr ~ "+" ~ multExpr  ^^ plus
    | addExpr ~ "-" ~ multExpr  ^^ minus
    | addExpr ~ "|" ~ multExpr  ^^ bitOr
    | addExpr ~ "^" ~ multExpr  ^^ bitXor
    | multExpr
    )
  
  lazy val multExpr: LrRule[Expr] =  "multiplicative expression: prec 5" $
    ( multExpr ~ "*"  ~ unaryExpr  ^^ times
    | multExpr ~ "/"  ~ unaryExpr  ^^ div
    | multExpr ~ "%"  ~ unaryExpr  ^^ mod
    | multExpr ~ "<<" ~ unaryExpr  ^^ shiftL
    | multExpr ~ ">>" ~ unaryExpr  ^^ shiftR
    | multExpr ~ "&"  ~ unaryExpr  ^^ bitAnd
    | multExpr ~ "&^" ~ unaryExpr  ^^ bitAndNot
    | unaryExpr
    )
  
  lazy val unaryExpr: LrRule[Expr] =          "unary expression: prec 6" $
    ( "+"  ~ unaryExpr  ^^ positive
    | "-"  ~ unaryExpr  ^^ negative
    | "^"  ~ unaryExpr  ^^ bitCompl
    | "!"  ~ unaryExpr  ^^ Combinators.not //find out which "not" the compiler was confused with.
    | "<-" ~ unaryExpr  ^^ chanRecv
    | "&"  ~ unaryExpr  ^^ addrOf
    | "*"  ~ unaryExpr  ^^ deref
    | primaryExpr
    )
    
  lazy val exprList: Rule[List[Expr]] =                "expression list" $
    rep1sep(expression, ",")
}
