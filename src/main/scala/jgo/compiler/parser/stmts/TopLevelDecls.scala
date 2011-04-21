package jgo.compiler
package parser.stmts

/*
trait TopLevelDecls extends Declarations with Blocks {
  lazy val functionOrMethodDecl: P_ =        "function or method declaration" $
    "func" ~>! ( funcDeclTail //make this committed! no backtracking to anything else if you see "func"
               | methodDeclTail
               )
  
  
  lazy val funcDeclTail: P_ =                         "rest of function decl" $
    ident ~ funcSignature ~ block.?
  
  
  lazy val methodDeclTail: P_ =                         "rest of method decl" $
    methodReciever ~ ident ~ funcSignature ~ block.?
  
  lazy val methodReciever: P_ =                        "method reciever spec" $
    "(" ~> ( methodRecieverType <~ ")"
           | ident ~ methodRecieverType <~ ")"
           )
  
  lazy val methodRecieverType: P_ =                    "method reciever type" $
    ( "*" ~> ident  //the base type name (second ident) must not be a pointer
    | ident         //or interf type and must be decl in same package as method
    )
  
  lazy val funcSignature: P_ =                           "function signature" $
    funcParams ~ funcResult.?
  
  lazy val funcResult: P_ =                            "function result-type" $
    ( goType
    | funcParams
    )
  
  lazy val funcParams: P_ =                             "function parameters" $
    "(" ~> repsep(funcParamDecl, ",") <~ ")" //recall that repsep admits zero repetitions
  
  lazy val funcParamDecl: P_ =            "function parameter(s) declaration" $
    ( identList ~ "..." ~ goType
    | identList ~ goType
    | goType
    )
}
*/
