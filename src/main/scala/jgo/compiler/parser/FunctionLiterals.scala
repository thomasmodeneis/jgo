package jgo.compiler
package parser

trait FunctionLiterals extends Statements {
  lazy val functionLit: P_ =                               "function literal" $
    functionType ~ block
}
