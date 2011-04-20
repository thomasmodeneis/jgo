package jgo.compiler
package parser.exprs

import scope._

import parser.types._
import parser.scoped._

import interm._
import interm.types._

trait PrimaryExprs extends Operands with TypeSyntax with Scoped with ExprUtils {
  self: Expressions =>
  
  lazy val primaryExpr: PP[Expr] =              "primary expression" $
    ( primaryExpr ~ selector                                                  //e.g. myStruct.field, or myPackage.value
    | primaryExpr ~ call                                                      //e.g. myFunc(param), int(5)
    | primaryExpr ~ index       ^^ mkIndex                                    //e.g. arr[0], myMap["hello"]
    | primaryExpr ~ slice                                                     //e.g. arr[2 : 5]
    | primaryExpr ~ typeAssert                                                //e.g. expr.(int)
    | "(" ~> expression <~ ")" //in general, "E = E ~ t2 | t1" MUST be used instead of "E = t1 | E ~ t2"
//  | (goType <~ "(") ~ expression <~ ")"                     &@ "unambiguous type conversion"
//  | specialBuiltinTypeCall //not yet supported
    | operand
    )
  
  lazy val selector: P[String] =          "field or method selector" $
    "." ~> ident
  
  lazy val index: P[Expr] =                        "subscript/index" $ 
    "[" ~> expression <~ "]"
  
  lazy val slice: P[Option[Expr] ~ Option[Expr]] = "slice operation" $
    "[" ~> (expression.? <~ ":") ~ expression.? <~ "]"
  
  lazy val typeAssert: P[Type] =                    "type assertion" $
    "." ~> "(" ~> goType <~ ")"
  
  lazy val call: P[List[Expr]] =    "function call" $
    "(" ~> exprList <~ ")"
    //"(" ~> exprList ~ "...".? <~ ")" //again with the optional trailing comma!
  
  
  def mkCall(func: Expr, args: List[Expr]): Expr
  
  def mkIndex(arr: Expr, indx: Expr): Expr =
    if (!indx.isOfType[IntegralType])
      badExpr("index not integral") //check what type is required (integral, unsigned, etc)
    else arr match {
      case HasType(ArrayType) => ArrayIndexLval(arr, indx)
      case HasType(SliceType) => SliceIndexLval(arr, indx)
      case _ => badExpr("not an array or slice")
    }
}
