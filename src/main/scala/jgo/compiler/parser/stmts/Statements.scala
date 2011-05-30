package jgo.compiler
package parser.stmts

import parser.exprs._
import parser.scoped._
import parser.funcs._

import interm._
import expr._
import expr.Combinators
import types._
import symbol._
import codeseq._
import instr._
import instr.TypeConversions._

/**
 * Provides the grammar and semantics of statements and blocks.
 */
trait Statements extends Expressions
                    with SimpleStmts
                    with Declarations
                    with FuncContext
                    with StackScoped
                    with Labels
                    with BreaksAndContinues {
  /**
   * For clarity, we use the name funcContext to refer to this Statements instance
   * when we use the members inherited from FuncContext.
   */
  funcContext: FuncContext =>
  
  /**
   * Returns `(new LocalVar(name, typeOf), Decl(<that variable>))`.
   * This method is required by Declarations.
   */
  protected def mkVariable(name: String, typeOf: Type): (Variable, CodeBuilder) = {
    val res = new LocalVar(name, typeOf)
    (res, Decl(res))
  }
  
  lazy val statement: Rule[CodeBuilder] =                                              "statement" $
    ( block
    | labeledStmt
    | ifStmt
    | loop(forStmt)
//  | breakable(switchStmt)
//  | breakable(selectStmt) //not yet supported in grammar
    | returnStmt
    | breakStmt
    | continueStmt
    | gotoStmt
//  | goStmt
//  | deferStmt
    | declaration
    | ident("print") ~ expression  ^^ procPrintStmt
    | simpleStmt  //contains the empty statement, so must come last
    | failure("not a statement")
    )
  
  def procPrintStmt(pos: Pos, eM: M[Expr]) =
    eM flatMap {
      case s OfType (StringType)     => Result(Combinators.eval(s) |+| PrintString)
      case n OfType (t: NumericType) => Result(Combinators.eval(n) |+| PrintNumeric(t))
      case HasType(t) => Problem("not a printable type: %s", t)(pos)
    }
  
  lazy val block: Rule[CodeBuilder] =                                                      "block" $
    scoped("{" ~> stmtList <~ "}")  ^^ makeBlock
  
  
  lazy val labeledStmt: Rule[CodeBuilder] =                                    "labeled statement" $
    label >> { nameAndLabel =>
      ( labeledLoop(forStmt)(nameAndLabel)
//    | labeledBreakable(switchStmt)(nameAndLabel)
//    | labeledBreakable(selectStmt)(nameAndLabel)
      | statement
      )
    }
  
  lazy val label: Parser[(String, M[UserLabel])]=                                          "label" $
    ident ~ ":"  ^^ defLabel 
  
  
  lazy val ifStmt: Rule[CodeBuilder] =                                              "if statement" $
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
  
  
  lazy val forStmt: Rule[(Label, Label) => CodeBuilder] =                          "for statement" $
    "for" ~>!
      (                       block   ^^ makeInfLoop
      | withPos(expression) ~ block   ^^ makeWhile
      |    scoped(forClause ~ block)  ^^ makeFor
//    |  scoped(rangeClause ~ block)
      )
  
  lazy val forClause =                                  "for-clause: ordinary, ternary for clause" $
    (simpleStmt.? <~ ";") ~ withPos(expression.? <~ ";") ~ simpleStmt.?
  
  lazy val rangeClause =                                         "range clause of a for statement" $
    expression ~ ("," ~> expression).? ~ (("=" | ":=") ~> "range" ~> expression)
  
  
//  lazy val selectStmt: PP =
//    "select" ~> "{" ~> rep(commClause) <~ "}"
//  lazy val commClause: PP =
  
  
  lazy val returnStmt: Rule[CodeBuilder] =                                      "return statement" $
    ( "return" ~ exprList  ^^ makeValueReturn
    | "return"             ^^ makeReturn
    )
  
  lazy val breakStmt: Rule[CodeBuilder] =                                        "break statement" $
    ( "break" ~ ident  ^^ procBreak
    | "break"          ^^ procBreak
    )
  
  lazy val continueStmt: Rule[CodeBuilder] =                                  "continue statement" $
    ( "continue" ~ ident  ^^ procContinue
    | "continue"          ^^ procContinue
    )
  
  lazy val gotoStmt: Rule[CodeBuilder] =                                          "goto statement" $
    "goto" ~! ident  ^^ procGoto
  
  
  lazy val goStmt =                                                                 "go statement" $
    "go" ~>! primaryExpr
  
  lazy val deferStmt =                                                           "defer statement" $
    "defer" ~>! primaryExpr
  
  
  lazy val stmtList: Rule[List[CodeBuilder]] =                                    "statement list" $
    repWithSemi(statement) ^^ { implicitly[List[M[CodeBuilder]] => M[List[CodeBuilder]]] }
  
  
  
  private def makeBlock(stmtsM: M[List[CodeBuilder]], undeclCode: CodeBuilder) =
    for (stmts <- stmtsM)
    yield (stmts foldLeft CodeBuilder()) { _ |+| _ } |+| undeclCode
  
  private def makeValueReturn(pos: Pos, lsM: M[List[Expr]]): M[CodeBuilder] =
    lsM flatMap { ls =>
      if (ls.length != funcContext.resultTypes.length)
        return Problem("wrong number of results %d, expected %d", ls.length, funcContext.resultTypes.length)(pos)
      
      var code = CodeBuilder.empty
      for (((e, t), i) <- ls zip resultTypes zipWithIndex) {
        if (!(t <<= e.t))
          return Problem("type %s of %s expression not assignable to corresponding result type %s",
                         e.t, ordinal(i), t)(pos)
        code = code |+| Combinators.eval(e)
      }
      code |+| ValueReturn
    }
  
  private def makeReturn(pos: Pos): M[CodeBuilder] = {
    if (funcContext.resultTypes != Nil && !funcContext.hasNamedResults)
      return Problem("expressionless return illegal, since function isn't void"
                     + " and doesn't have named results")(pos)
    Result(Return)
  }
  
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
      bool <- Combinators.boolean(cond)(condPos)
    } yield init |+| (els match {
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
      bool <- Combinators.boolean(cond)(condPos)
    } yield
      bool.mkWhile(body) _
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
          bool <- Combinators.boolean(cond)(condPos)
        } yield { (brk: Label, cont: Label) =>
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
