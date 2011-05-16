package jgo.compiler
package parser.stmts

import parser.exprs._
import parser.scoped._
import parser.funcs._

import interm._
import expr._
import expr.{Combinators => C}
import types._
import symbol._
import codeseq._
import instr._

trait Statements extends Expressions
                    with SimpleStmts
                    with Declarations
                    with BreaksAndContinues
                    with StackScoped
                    with StmtUtils {
  //self: FuncContext =>
  
  /**
   * Returns `(new LocalVar(name, typeOf), Decl(<that variable>))`.
   * This method is required by Declarations.
   */
  protected def mkVariable(name: String, typeOf: Type): (Variable, CodeBuilder) = {
    val res = new LocalVar(name, typeOf)
    (res, Decl(res))
  }
  
  lazy val statement: PM[CodeBuilder] =                                                "statement" $
    ( block
//  | labeledStmt
    | ifStmt
    | loop(forStmt)
//  | breakable(switchStmt)
//  | breakable(selectStmt) //not yet supported in grammar
//  | goStmt
//  | returnStmt
//  | breakStmt
//  | continueStmt
//  | gotoStmt
//  | deferStmt
    | declaration
    | simpleStmt  //contains the empty statement, so must come last
    | failure("not a statement")
    )
  
  lazy val block: PM[CodeBuilder] =                                                        "block" $
    scoped("{" ~> stmtList <~ "}")  ^^ makeBlock
  
  /*
  lazy val labeledStmt: P_ =                                                   "labeled statement" $
    guard(ident <~ ":") ~>!
      (
      | statement
      )*/
  
  lazy val ifStmt: PM[CodeBuilder] =                                                "if statement" $
    "if" ~>! scoped(
      (simpleStmt <~ ";").? ~ withPos(expression) ~ block ~ ("else" ~>! statement).?
    )  ^^ makeIfStmt
  
  lazy val switchStmt: P_ =                                                     "switch statement" $
    "switch" ~>!
       ( exprSwitchStmtTail
       | typeSwitchStmtTail
       )
  
  lazy val exprSwitchStmtTail: P_ =                             "expression switch statement tail" $
    opt(simpleStmt <~ ";") ~ opt(expression) ~ ("{" ~> rep(exprCaseClause) <~ "}")
  
  lazy val exprCaseClause: P_ =                                    "expression switch case clause" $
    ( "case" ~> exprList ~ (":" ~> stmtList) ~ opt("fallthrough" <~ ";")
    | "default"          ~ (":" ~> stmtList) ~ opt("fallthrough" <~ ";")
    )
  
  lazy val typeSwitchStmtTail: P_ =                                   "type switch statement tail" $
    opt(simpleStmt <~ ";") ~ typeSwitchGuard ~ ("{" ~> rep(typeCaseClause) <~ "}")
  
  lazy val typeSwitchGuard: P_ =                                     "type switch statement guard" $
    opt(ident <~ ":=") ~ primaryExpr <~ "." <~ "(" <~ "type" <~ ")"
  
  lazy val typeCaseClause: P_ =                                          "type switch case clause" $
    ( "case" ~> typeList ~ (":" ~> stmtList) //make sure "nil" can be in the type list
    | "default"          ~ (":" ~> stmtList)
    )
  
  lazy val forStmt: PM[(Label, Label) => CodeBuilder] =                            "for statement" $
    "for" ~>!
      (                               block   ^^ makeInfLoop
      |        withPos(expression)  ~ block   ^^ makeWhile
      | scoped(forClause            ~ block)  ^^ makeFor
//    | (rangeClause ~ block  &@ "for with range clause")
      )
  
  lazy val forClause =                                  "for-clause: ordinary, ternary for clause" $
    (simpleStmt.? <~ ";") ~ withPos(expression.? <~ ";") ~ simpleStmt.?
  
  lazy val rangeClause: P_ =                                     "range clause of a for statement" $
    expression ~ ("," ~> expression).? ~ (("=" | ":=") ~> "range" ~> expression)
  
//  lazy val selectStmt: PP =
//    "select" ~> "{" ~> rep(commClause) <~ "}"
//  lazy val commClause: PP =
  
  lazy val goStmt: P_ =                                                             "go statement" $
    "go" ~>! primaryExpr
  
  lazy val returnStmt: P_ =                                                     "return statement" $
    "return" ~>! exprList.?
  
  lazy val breakStmt: PM[CodeBuilder] =                                          "break statement" $
    ( "break" ~ ident  ^^ procBreak
    | "break"          ^^ procBreak
    )
  
  lazy val continueStmt: P_ =                                                 "continue statement" $
    "continue" ~>! ident.?
  
  lazy val gotoStmt: P_ =                                                         "goto statement" $
    "goto" ~>! ident
  
  lazy val deferStmt: P_ =                                                       "defer statement" $
    "defer" ~>! primaryExpr
  
  lazy val stmtList: PM[List[CodeBuilder]] =                                      "statement list" $
    repWithSemi(statement) ^^ { implicitly[List[M[CodeBuilder]] => M[List[CodeBuilder]]] }
  
  
  
  private def takeCode(tuple: (CodeBuilder, Label)) = tuple._1
  private def takeCode(tuple: (CodeBuilder, Label, Label)) = tuple._1
  
//  private implicit def ls2code(ls: List[CodeBuilder]): CodeBuilder =
//    ls reduceLeft { _ |+| _ }
  
  private def makeBlock(stmtsM: M[List[CodeBuilder]], undeclCode: CodeBuilder) =
    for (stmts <- stmtsM)
    yield (stmts foldLeft CodeBuilder()) { _ |+| _ } |+| undeclCode
  
  private def makeIfStmt(
      initUgly:   Option[M[CodeBuilder]],
      condWPos:   (M[Expr], Pos),
      bodyM:      M[CodeBuilder],
      elseUgly:   Option[M[CodeBuilder]],
      undeclCode: CodeBuilder
  ): M[CodeBuilder] = {
    val initM: M[Option[CodeBuilder]] = initUgly
    val elseM: M[Option[CodeBuilder]] = elseUgly
    val (condM, condPos) = condWPos
    
    for {
      (init, cond, body, els) <- (initM, condM, bodyM, elseM)
      bool <- C.boolean(cond)(condPos)
    }
    yield init |+| (els match {
      case None           => bool.mkIf(body)
      case Some(elseCode) => bool.mkIfElse(body, elseCode)
    }) |+| undeclCode
  }
  
  private def makeInfLoop(bodyM: M[CodeBuilder]) = 
    for (body <- bodyM)
    yield (brk: Label, cont: Label) => Lbl(cont) |+| body |+| Goto(cont) |+| Lbl(brk)
  
  private def makeWhile(condWPos: (M[Expr], Pos), bodyM: M[CodeBuilder]) = {
    val (condM, condPos) = condWPos
    for {
      (cond, body) <- (condM, bodyM)
      bool <- C.boolean(cond)(condPos)
    }
    yield bool.mkWhile(body) _
  }
  
  private def makeFor(
      initUgly:   Option[M[CodeBuilder]],
      condWPos:   (Option[M[Expr]], Pos),
      incrUgly:   Option[M[CodeBuilder]],
      bodyM:      M[CodeBuilder],
      undeclCode: CodeBuilder
  ) = {
    val initM: M[Option[CodeBuilder]] = initUgly
    val incrM: M[Option[CodeBuilder]] = incrUgly
    val (condUgly, condPos) = condWPos
    
    condUgly match {
      case Some(condM) =>
        for {
          (init, cond, incr, body) <- (initM, condM, incrM, bodyM)
          bool <- C.boolean(cond)(condPos)
        }
        yield { (brk: Label, cont: Label) =>
          //an (old) implicit conversion turns incr into a CodeBuilder
          //In the event of None, empty code. Also init.
          init |+| bool.mkFor(body, incr)(brk, cont) |+| undeclCode
        }
      
      case None =>
        for ((init, incr, body) <- (initM, incrM, bodyM))
        yield { (brk: Label, cont: Label) =>
          val top  = new Label("top of forever")
          init |+| Lbl(top) |+| body |+| Lbl(cont) |+| incr |+| Goto(top) |+| Lbl(brk) |+| undeclCode
        }
    }
  }
}
