package jgo.compiler
package parser.stmts

import parser.exprs._
import parser.scoped._

import interm._
import codeseq._
import instr._

trait Statements extends Expressions with SimpleStmts with Declarations with StackScoped with StmtUtils {
  lazy val statement: PM[CodeBuilder] =                           "statement" $
    ( block
//    | labeledStmt
    | ifStmt
//    | switchStmt
    | forStmt
//    | selectStmt  //not yet supported
//    | goStmt
//    | returnStmt
//    | breakStmt
//    | continueStmt
//    | gotoStmt
//    | deferStmt
    | declaration
    | simpleStmt  //contains the empty statement, so must come last
    )
  
  lazy val block: PM[CodeBuilder] =                                   "block" $
    scoped("{" ~> stmtList <~ "}")  ^^ makeBlock
  
  lazy val labeledStmt: P_ =                              "labeled statement" $
    (ident <~ ":") ~ statement
  
  lazy val ifStmt: PM[CodeBuilder] =                           "if statement" $
    "if" ~>!
      scoped((simpleStmt <~ ";").? ~ expression ~ block ~ ("else" ~>! statement).?)  ^^ makeIfStmt
  
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
  
  lazy val forStmt: PM[CodeBuilder] =                         "for statement" $
    "for" ~>!
      ( (block                &@ "for with no clause: forever")            ^^ makeInfLoop
      | (expression  ~ block  &@ "for with while-esq conditional clause")  ^^ makeWhile
      | scoped(forClause   ~ block  &@ "for with ordinary for-clause")     ^^ makeFor
//      | (rangeClause ~ block  &@ "for with range clause")
      )
  
  lazy val forClause =             "for-clause: ordinary, ternary for clause" $
    (simpleStmt.? <~ ";") ~ (expression.? <~ ";") ~ simpleStmt.?
  
  lazy val rangeClause: P_ =                "range clause of a for statement" $
    expression ~ ("," ~> expression).? ~ (("=" | ":=") ~> "range" ~> expression)
  
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
  
  lazy val stmtList: PM[List[CodeBuilder]] =                 "statement list" $
    repWithSemi(statement) ^^ { implicitly[List[M[CodeBuilder]] => M[List[CodeBuilder]]] }
  
  
  
//  private implicit def ls2code(ls: List[CodeBuilder]): CodeBuilder =
//    ls reduceLeft { _ |+| _ }
  
  private def makeBlock(stmtsM: M[List[CodeBuilder]], undeclCode: CodeBuilder): M[CodeBuilder] =
    for (stmts <- stmtsM)
    yield (stmts foldLeft CodeBuilder())(_ |+| _) |+| undeclCode
  
  private def makeIfStmt(
    init:       Option[CodeBuilder],
    cond:       Expr,
    body:       CodeBuilder,
    els:        Option[CodeBuilder],
    undeclCode: CodeBuilder
  ): CodeBuilder = cond match {
    
    case bool: BoolExpr =>
      init |+|
      (els match {
        case None           => bool.mkIf(body)
        case Some(elseCode) => bool.mkIfElse(body, elseCode)
      }) |+|
      undeclCode
      
    case _ => badStmt("condition of if statement not a boolean expression")
  }
  
  private def makeInfLoop(body: CodeBuilder): CodeBuilder = {
    val top = new Label("top of unconditional for loop")
    Lbl(top) |+| body |+| Goto(top)
  }
  
  private def makeWhile(cond: Expr, body: CodeBuilder) = cond match {
    case bool: BoolExpr => bool.mkWhile(body)
    case _ => badStmt("condition of for statement not a boolean expression")
  }
  
  private def makeFor(
    init:       Option[CodeBuilder],
    cond:       Option[Expr],
    incrStmt:   Option[CodeBuilder],
    body:       CodeBuilder,
    undeclCode: CodeBuilder
  ): CodeBuilder = cond match {
    
    case Some(bool: BoolExpr) =>
      init |+|
      bool.mkWhile(body |+| incrStmt) |+|
      undeclCode
      
    case Some(_) => badStmt("condition of for statement not a boolean expression")
    
    case None => init |+| makeInfLoop(body |+| incrStmt) |+| undeclCode
  }
}
