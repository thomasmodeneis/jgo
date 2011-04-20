package jgo.compiler
package parser

import interm._
import types._

trait CompositeLiterals extends TypeSyntax {
  self: Expressions =>
  
  lazy val compositeLit: PP_ =                     "composite literal" $
    compLitType ~ compLitValue
  
  lazy val compLitType: P_ =        "type-clause of composite literal" $
    ( structType
    | arrayType
    | ("[" ~ "..." ~ "]") ~> goType
    | sliceType
    | mapType
    | typeSymbol  //must refer to one of the above kinds of type
    )
  
  lazy val compLitValue: PP_ =     "value-clause of composite literal" $
    "{" ~> repsep(compLitElem, ",") <~ "}"
     //bafflingly, spec says "{" ~ (elemList ~ ","?)? ~ "}"
  
  lazy val compLitElem: PP_ =              "composite literal element" $
    ( compLitElemKey ~ ":" ~ compLitElemValue
    | compLitElemValue
    )
  
  lazy val compLitElemKey: PP_ =       "composite literal element key" $
    ( ident //struct field name
    | expression  //index (incl. map "index")
    )
  
  lazy val compLitElemValue: PP_ =   "composite literal element value" $
    ( expression
    | compLitValue
    )
}
