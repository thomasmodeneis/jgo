package jgo.compiler
package parser

trait SimpleStmts extends Expressions with Symbols with GrowablyScoped {
  lazy val simpleStmt: P_ =                           "simple statement" $ //was PP; not sure why
    ( assignment
    | shortVarDecl
    | incOrDecStmt
    | sendStmt
    | expression   //expression statement
    | guard(";")   //the empty statement
    )
  
  lazy val sendStmt: P_ =                               "send statement" $
    (expression <~ "<-") ~ expression //the first expr must be of channel type
  
  lazy val incOrDecStmt: P_ =         "increment or decrement statement" $
    ( expression ~ ("++" | "--" | failure("`++' or `--' expected"))
    )
  
  lazy val assignment: P_ =                       "assignment statement" $
    ( (exprList   <~ "=")   ~ exprList
    | (expression <~ "+=")  ~ expression
    | (expression <~ "-=")  ~ expression
    | (expression <~ "|=")  ~ expression
    | (expression <~ "^=")  ~ expression
    | (expression <~ "*=")  ~ expression
    | (expression <~ "/=")  ~ expression
    | (expression <~ "%=")  ~ expression
    | (expression <~ "<<=") ~ expression
    | (expression <~ ">>=") ~ expression
    | (expression <~ "&=")  ~ expression
    | (expression <~ "&^=") ~ expression
    )
  
  lazy val shortVarDecl: P_ =               "short variable declaration" $ //may appear only inside a function
    identList ~ ":=" ~ exprList                              //the number of idents must = the number of exprs
  
  /*lazy val lvalue: P_ =                                         "lvalue" $
    ( varSymbol
    | err("not an lvalue; lvalues restricted to vars for the time being")
    )
  
  lazy val lvalList: P_ =                                  "lvalue list" $
    rep1sep(lvalue, ",")*/
}
