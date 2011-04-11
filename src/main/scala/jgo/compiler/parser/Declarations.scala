package jgo.compiler
package parser

trait Declarations extends Expressions with GrowablyScoped {
  lazy val declaration: P_ =                                    "declaration" $
    constDecl | typeDecl | varDecl
  
  lazy val constDecl: P_ =                                "const declaration" $
    "const" ~>! ( constSpec
                | "(" ~> repWithSemi(constSpec) <~ ")"
                )
  lazy val constSpec: P_ =                                  "const decl spec" $
    ( identList ~ "=" ~ exprList               //note the first spec in a constDecl may not have form `idList'
    | identList ~ goType ~ "=" ~ exprList      //the number of idents must = the number of exprs
    | identList                                //don't forget about iota
    )
  
  lazy val typeDecl: P_ =                                  "type declaration" $
    "type" ~>! ( typeSpec
               | "(" ~> repWithSemi(typeSpec) <~ ")"
               )
  lazy val typeSpec: P_ =                                    "type decl spec" $
    ident ~ goType
  
  lazy val varDecl: P_ =                                    "var declaration" $
    "var" ~> commit( varSpec
                   | "(" ~> repWithSemi(varSpec) <~ ")"
                   )
  lazy val varSpec: P_ =                                      "var decl spec" $
    ( identList          ~ "=" ~ exprList //the number of idents must = the number of exprs
    | identList ~ goType ~ "=" ~ exprList //the number of idents must = the number of exprs
    | identList ~ goType
    )
}
