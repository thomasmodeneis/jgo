package jgo.compiler
package parser.stmts

import parser.exprs._
import parser.scoped._

import interm._
import expr._
import expr.{Combinators => C}
import codeseq._
import instr._
import instr.TypeConversions._
import types._
import symbol._

trait SimpleStmts extends Expressions with Symbols with GrowablyScoped with ExprUtils with StmtUtils {
  lazy val simpleStmt: PM[CodeBuilder] =                          "simple statement" $
    ( assignment
    | shortVarDecl
    | incOrDecStmt
    | sendStmt
    | expression  ^^ map(C.eval)
    | success(CodeBuilder.empty) //empty stmt
    )
  
  lazy val sendStmt: PM[CodeBuilder] =                              "send statement" $
    expression ~ "<-" ~ expression  ^^ C.chanSend
  
  lazy val incOrDecStmt: PM[CodeBuilder] =        "increment or decrement statement" $
    ( expression ~ "++"  ^^ convSuffix(C.incr) //Not sure why the conversion isn't being applied implicitly,
    | expression ~ "--"  ^^ convSuffix(C.decr) //but don't have the time to find out right now.
  //| failure("`++' or `--' expected")         
    )
  
  lazy val assignment: PM[CodeBuilder] =                      "assignment statement" $
    ( exprList ~ "=" ~ exprList  ^^ C.assign
  /*| expression ~ "+="  ~ expression
    | expression ~ "-="  ~ expression
    | expression ~ "|="  ~ expression
    | expression ~ "^="  ~ expression
    | expression ~ "*="  ~ expression
    | expression ~ "/="  ~ expression
    | expression ~ "%="  ~ expression
    | expression ~ "<<=" ~ expression
    | expression ~ ">>=" ~ expression
    | expression ~ "&="  ~ expression
    | expression ~ "&^=" ~ expression*/
    )
  
  lazy val shortVarDecl: PM[CodeBuilder] =              "short variable declaration" $
    identPosList ~ ":=" ~ exprList  ^^ declAssign
  
  
  private def declAssign(left: List[(String, Pos)], eqPos: Pos, rightM: M[List[Expr]]): M[CodeBuilder] = 
    rightM flatMap { right =>
      var declCode = CodeBuilder.empty
      var actuallySawDecl = false
      
      val leftVarsM: M[List[Variable]] = //implicit conv
        for (((l, pos), r) <- left zip right)
        yield
          if (!growable.alreadyDefined(l)) { //not already defined in innermost scope
            actuallySawDecl = true
            val v = new LocalVar(l, r.t)
            growable.put(l, v)
            declCode = declCode |+| Decl(v)
            M(v)
          }
          else
            getVariable(l, pos)
      
      if (actuallySawDecl)
        for {
          leftVars <- leftVarsM
          assignCode <- C.assign(leftVars map varLval, right)(eqPos)
        } yield declCode |+| assignCode
      else
        Problem("no new variables on left side of :=")(eqPos)
    }
}
