package jgo.compiler
package parser.stmts

/*
trait TopLevelDecls extends Declarations {
  lazy val functionOrMethodDecl: P_ =        "function or method declaration" $
    "func" ~>! ( funcDeclTail //make this committed! no backtracking to anything else if you see "func"
               //| methodDeclTail
               )
  
  
  private lazy val funcDeclTail: P_ =                 "rest of function decl" $
    ident ~ funcSignature ~ block.?
  
  
  private lazy val methodDeclTail: P_ =                 "rest of method decl" $
    methodReciever ~ ident ~ funcSignature ~ block.?
  
  private lazy val methodReciever: P_ =                "method reciever spec" $
    "(" ~> ( methodRecieverType <~ ")"
           | ident ~ methodRecieverType <~ ")"
           )
  
  private lazy val methodRecieverType: P_ =            "method reciever type" $
    ( "*" ~> ident  //the base type name (second ident) must not be a pointer
    | ident         //or interf type and must be decl in same package as method
    )
  
  private lazy val funcSignature: P_ =                   "function signature" $
    funcParams ~ funcResult.?
  
  private lazy val funcResult: P_ =                    "function result-type" $
    ( goType
    | funcParams
    )
  
  private lazy val funcParams: P_ =                     "function parameters" $
    "(" ~> repsep(funcParamDecl, ",") <~ ")" //recall that repsep admits zero repetitions
  
  private lazy val funcParamDecl: P_ =    "function parameter(s) declaration" $
    ( identList ~ "..." ~ goType
    | identList ~ goType
    | goType
    )
}
*/
