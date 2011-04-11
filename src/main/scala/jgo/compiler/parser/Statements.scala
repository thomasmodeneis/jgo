package jgo.compiler
package parser

trait Statements extends Expressions with StackScoped with SimpleStmts with Declarations {
  lazy val statement: P_ =                                       "statement" $
    ( block
    | labeledStmt
    | simpleStmt
    | ifStmt
    | switchStmt
    | forStmt
 // | selectStmt  //not yet supported
    | goStmt
    | returnStmt
    | breakStmt
    | continueStmt
    | gotoStmt
    | deferStmt
    | declaration
    )
  
  lazy val block: P_ =                                               "block" $
    "{" ~> stmtList <~ "}"
  
  lazy val labeledStmt: P_ =                             "labeled statement" $
    (ident <~ ":") ~ statement
  
  lazy val ifStmt: P_ =                                       "if statement" $
    ( "if" ~>!
        ( (simpleStmt <~ ";") ~ expression ~ block
        | (simpleStmt <~ ";")              ~ block
        |                       expression ~ block
        |                                    block
        )
    ~ ("else" ~>! statement).?
    )
  
  lazy val switchStmt: P_ =                                "switch statement" $
    "switch" ~>!
       ( exprSwitchStmtTail
       | typeSwitchStmtTail
       )
  
  lazy val exprSwitchStmtTail: P_ =        "expression switch statement tail" $
    opt(simpleStmt <~ ";") ~ opt(expression) ~ ("{" ~> rep(exprCaseClause) <~ "}")
  
  lazy val exprCaseClause: P_ =               "expression switch case clause" $
    ( "case" ~> exprList ~ (":" ~> stmtList) ~ opt("fallthrough" <~ ";")
    | "default"          ~ (":" ~> stmtList) ~ opt("fallthrough" <~ ";")
    )
  
  lazy val typeSwitchStmtTail: P_ =              "type switch statement tail" $
    opt(simpleStmt <~ ";") ~ typeSwitchGuard ~ ("{" ~> rep(typeCaseClause) <~ "}")
  
  lazy val typeSwitchGuard: P_ =                "type switch statement guard" $
    opt(ident <~ ":=") ~ primaryExpr <~ "." <~ "(" <~ "type" <~ ")"
  
  lazy val typeCaseClause: P_ =                     "type switch case clause" $
    ( "case" ~> typeList ~ (":" ~> stmtList) //make sure "nil" can be in the type list
    | "default"          ~ (":" ~> stmtList)
    )
  
  lazy val forStmt: P_ =                                      "for statement" $
    "for" ~>!
      ( (block                &@ "for with no clause: forever")
      | (expression  ~ block  &@ "for with while-esq conditional clause")
      | (forClause   ~ block  &@ "for with ordinary for-clause")
      | (rangeClause ~ block  &@ "for with range clause")
      )
  lazy val forClause: P_ =         "for-clause: ordinary, ternary for clause" $
    (opt(simpleStmt) <~ ";") ~ (opt(expression) <~ ";") ~ simpleStmt?
  lazy val rangeClause: P_ =                "range clause of a for statement" $
    expression ~ opt("," ~> expression) ~ (("=" | ":=") ~> "range" ~> expression)
  
//  lazy val selectStmt: PP =
//    "select" ~> "{" ~> rep(commClause) <~ "}"
//  lazy val commClause: PP =
  
  lazy val goStmt: P_ =                                        "go statement" $
    "go" ~>! primaryExpr
  
  lazy val returnStmt: P_ =                                "return statement" $
    "return" ~>! exprList.?
  
  lazy val breakStmt: P_ =                                  "break statement" $
    "break" ~>! ident.?
  
  lazy val continueStmt: P_ =                            "continue statement" $
    "continue" ~>! ident.?
  
  lazy val gotoStmt: P_ =                                    "goto statement" $
    "goto" ~>! ident
  
  lazy val deferStmt: P_ =                                  "defer statement" $
    "defer" ~>! primaryExpr
  
  lazy val stmtList: P_ =                                    "statement list" $
    repWithSemi(statement)
}
