package jgo.compiler
package parser.exprs

import parser.stmts._

trait FunctionLiterals extends Statements {
  lazy val functionLit: P_ =                               "function literal" $
    funcType ~ block
}
