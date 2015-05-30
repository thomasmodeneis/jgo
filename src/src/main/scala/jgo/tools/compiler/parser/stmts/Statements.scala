package jgo.tools.compiler
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
    catchSyntaxErr("not a statement",
      ( block
      | labeledStmt
      | ifStmt
      | loop(forStmt)
//    | breakable(switchStmt)
//    | breakable(selectStmt) //not yet supported in grammar
      | returnStmt
      | breakStmt
      | continueStmt
      | gotoStmt
//    | goStmt
//    | deferStmt
      | declaration
      | ident("print") ~ expression  ^^ procPrintStmt
      | simpleStmt  //contains the empty statement, so must come last
      )
    )
  
  /** temporary hack to permit testing of generated programs */
  private def procPrintStmt(pos: Pos, eErr: Err[Expr]) =
    eErr flatMap {
      case s OfType (StringType)     => result(Combinators.eval(s)    |+| PrintString)
      case n OfType (t: NumericType) => result(Combinators.eval(n)    |+| PrintNumeric(t))
      case u: UntypedConst           => result(PushStr(u.valueString) |+| PrintString) //hack within a hack!
      case HasType(t) => problem("not a printable type: %s", t)(pos)
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
  
  lazy val label: Parser[(String, Err[UserLabel])]=                                          "label" $
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
    repWithSemi(statement) ^^ { implicitly[List[Err[CodeBuilder]] => Err[List[CodeBuilder]]] }
  
  
  
  private def makeBlock(stmtsErr: Err[List[CodeBuilder]], undeclCode: CodeBuilder) =
    for (stmts <- stmtsErr)
    yield (stmts foldLeft CodeBuilder()) { _ |+| _ } |+| undeclCode
  
  private def makeValueReturn(pos: Pos, esErr: Err[List[Expr]]): Err[CodeBuilder] =
    esErr flatMap { ls =>
      if (ls.length != funcContext.resultTypes.length)
        return problem("wrong number of results %d, expected %d", ls.length, funcContext.resultTypes.length)(pos)
      
      var code = CodeBuilder.empty
      for (((e, t), i) <- ls zip resultTypes zipWithIndex) {
        if (!(t <<= e.typeOf))
          return problem("type %s of %s expression not assignable to corresponding result type %s",
                         e.typeOf, ordinal(i), t)(pos)
        code = code |+| Combinators.eval(e)
      }
      result(code |+| ValueReturn)
    }
  
  private def makeReturn(pos: Pos): Err[CodeBuilder] = {
    if (funcContext.resultTypes != Nil && !funcContext.hasNamedResults)
      return problem("expressionless return illegal, since function isn't void"
                     + " and doesn't have named results")(pos)
    result(Return)
  }
  
  private def makeIfStmt(
      initUgly:    Option[Err[CodeBuilder]],
      testErrWPos: (Err[Expr], Pos),
      bodyErr:     Err[CodeBuilder],
      elseUgly:    Option[Err[CodeBuilder]],
      undeclCode:  CodeBuilder
  ): Err[CodeBuilder] = {
    val initErr = Err.liftOpt(initUgly)
    val elseErr = Err.liftOpt(elseUgly)
    val (testErr, testPos) = testErrWPos
    
    for {
      (init, test, body, els) <- (initErr, testErr, bodyErr, elseErr)
      cond <- Combinators.conditional(test)(testPos)
    } yield init |+| (els match {
      case None           => cond.mkIf(body)
      case Some(elseCode) => cond.mkIfElse(body, elseCode)
    }) |+| undeclCode
  }
  
  private def makeInfLoop(bodyErr: Err[CodeBuilder]) = 
    for (body <- bodyErr)
    yield (brk: Label, cont: Label) => Lbl(cont) |+| body |+| Goto(cont) |+| Lbl(brk)
  
  private def makeWhile(testErrWPos: (Err[Expr], Pos), bodyErr: Err[CodeBuilder]) = {
    val (testErr, testPos) = testErrWPos
    for {
      (test, body) <- (testErr, bodyErr)
      cond <- Combinators.conditional(test)(testPos)
    } yield cond.mkWhile(body) _
  }
  
  private def makeFor(
      initUgly:     Option[Err[CodeBuilder]],
      testUglyWPos: (Option[Err[Expr]], Pos),
      incrUgly:     Option[Err[CodeBuilder]],
      bodyErr:      Err[CodeBuilder],
      undeclCode:   CodeBuilder
  ) = {
    val initErr = Err.liftOpt(initUgly)
    val incrErr = Err.liftOpt(incrUgly)
    val (testUgly, testPos) = testUglyWPos
    
    testUgly match {
      case Some(testErr) =>
        for {
          (init, test, incr, body) <- (initErr, testErr, incrErr, bodyErr)
          cond <- Combinators.conditional(test)(testPos)
        } yield { (brk: Label, cont: Label) =>
          //an (old) implicit conversion turns incr into a CodeBuilder
          //In the event of None, empty code. Also init.
          init |+| cond.mkFor(body, incr)(brk, cont) |+| undeclCode
        }
      
      case None =>
        for ((init, incr, body) <- (initErr, incrErr, bodyErr))
        yield { (brk: Label, cont: Label) =>
          val top  = new Label("top of forever")
          init |+| Lbl(top) |+| body |+| Lbl(cont) |+| incr |+| Goto(top) |+| Lbl(brk) |+| undeclCode
        }
    }
  }
}
