package jgo.compiler
package parser
package exprs

import parser.types._

trait FunctionLiterals extends Types {
  /** TypeSyntax requires this selftype so it can process the length part of array types. */
  self: Expressions =>
  //lazy val functionLit: P_ =                               "function literal" $
    //funcType ~ block
}
