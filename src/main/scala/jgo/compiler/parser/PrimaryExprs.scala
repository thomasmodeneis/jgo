package jgo.compiler
package parser

import scope._

import interm._
import types._

trait PrimaryExprs extends Operands with TypeSyntax {
  self: Expressions =>
  
  lazy val primaryExpr: PP_ =                   "primary expression" $
    ( primaryExpr ~ selector                                                  //e.g. myStruct.field, or myPackage.value
    | primaryExpr ~ call                                                      //e.g. myFunc(param), int(5)
    | primaryExpr ~ index                                                     //e.g. arr[0], myMap["hello"]
    | primaryExpr ~ slice                                                     //e.g. arr[2 : 5]
    | primaryExpr ~ typeAssert                                                //e.g. expr.(int)
    | "(" ~> expression <~ ")" //in general, "E = E ~ t2 | t1" MUST be used instead of "E = t1 | E ~ t2"
    | (goType <~ "(") ~ expression <~ ")"                     &@ "unambiguous type conversion"
    | specialBuiltinTypeCall
    | literal
    | ident //yes, this *must* be last, to prevent preemptive prefix-matching (I hope you/I remember what this means!)
    )
  
  lazy val selector: P[String] =          "field or method selector" $
    "." ~> ident
  
  lazy val index: P_      =                        "subscript/index" $ 
    "[" ~> expression <~ "]"
  
  lazy val slice: P[Option[_] ~ Option[_]] =       "slice operation" $
    "[" ~> (expression.? <~ ":") ~ expression.? <~ "]"
  
  lazy val typeAssert: P[Type] =                    "type assertion" $
    "." ~> "(" ~> goType <~ ")"
  
  lazy val call: P[List[_] ~ Option[String]] =       "function call" $
    "(" ~> exprList ~ "...".? <~ ")" //again with the optional trailing comma!
}
