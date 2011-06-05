package jgo.compiler
package parser.stmts

import parser.exprs._
import parser.scoped._

import interm._
import expr._
import expr.Combinators
import codeseq._
import instr._
import instr.TypeConversions._
import types._
import symbol._

/**
 * Provides the grammar and semantics of simple statements.
 * The following statements are considered simple:
 * <ul>
 * <li>Assignment statements (+=, etc. not currently implemented)</li>
 * <li>Short variable declarations</li>
 * <li>Increment and decrement statements</li>
 * <li>Send statements</li>
 * <li>Expressions statements</li>
 * <li>The empty statement</li>
 * </ul>
 */
trait SimpleStmts extends Expressions with Symbols with GrowablyScoped with ExprUtils {
  lazy val simpleStmt: Rule[CodeBuilder] =                          "simple statement" $
    ( assignment
    | shortVarDecl
    | incOrDecStmt
    | sendStmt
    | expression  ^^ map(Combinators.eval)
    | success(result(CodeBuilder.empty)) //empty stmt
    )
  
  lazy val sendStmt: Rule[CodeBuilder] =                              "send statement" $
    expression ~ "<-" ~ expression  ^^ Combinators.chanSend
  
  lazy val incOrDecStmt: Rule[CodeBuilder] =        "increment or decrement statement" $
    ( expression ~ "++"  ^^ convSuffix(Combinators.incr) //Not sure why the conversion isn't being applied implicitly,
    | expression ~ "--"  ^^ convSuffix(Combinators.decr) //but don't have the time to find out right now.
    )
  
  lazy val assignment: Rule[CodeBuilder] =                      "assignment statement" $
    ( exprList ~ "=" ~ exprList  ^^ Combinators.assign
    | expression ~ "+="  ~ expression  ^^ opAssign(Combinators.plus)
    | expression ~ "-="  ~ expression  ^^ opAssign(Combinators.minus)
    | expression ~ "|="  ~ expression  ^^ opAssign(Combinators.bitOr)
    | expression ~ "^="  ~ expression  ^^ opAssign(Combinators.bitXor)
    | expression ~ "*="  ~ expression  ^^ opAssign(Combinators.times)
    | expression ~ "/="  ~ expression  ^^ opAssign(Combinators.div)
    | expression ~ "%="  ~ expression  ^^ opAssign(Combinators.mod)
    | expression ~ "<<=" ~ expression  ^^ opAssign(Combinators.shiftL)
    | expression ~ ">>=" ~ expression  ^^ opAssign(Combinators.shiftR)
    | expression ~ "&="  ~ expression  ^^ opAssign(Combinators.bitAnd)
    | expression ~ "&^=" ~ expression  ^^ opAssign(Combinators.bitAndNot)
    )
  
  lazy val shortVarDecl: Rule[CodeBuilder] =              "short variable declaration" $
    identPosList ~ ":=" ~ exprList  ^^ declAssign
  
  
  private def opAssign(f: (Expr, Expr) => Pos => Err[Expr])(leftErr: Err[Expr], pos: Pos, rightErr: Err[Expr]) =
    for {
      (left, right) <- (leftErr, rightErr)
      target <- f(left, right)(pos)
      result <- Combinators.assign(left, target)(pos)
    } yield result
  
  private def declAssign(left: List[(String, Pos)], eqPos: Pos, rightErr: Err[List[Expr]]): Err[CodeBuilder] = 
    rightErr flatMap { right =>
      var declCode = CodeBuilder.empty
      var actuallySawDecl = false
      
      if (left.length != right.length)
        return problem("arity (%d) of left side of := unequal to arity (%d) of right side",
                       left.length, right.length)(eqPos)
      
      val leftVarsUgly =
        for (((l, pos), r) <- left zip right)
        yield
          if (!growable.alreadyDefined(l)) { //not already defined in innermost scope
            actuallySawDecl = true
            val v = new LocalVar(l, r.inferenceType)
            growable.put(l, v)
            declCode = declCode |+| Decl(v)
            result(v)
          }
          else
            getVariable(l, pos)
      
      if (actuallySawDecl)
        for {
          leftVars <- Err.liftList(leftVarsUgly)
          assignCode <- Combinators.assign(leftVars map Combinators.fromVariable, right)(eqPos)
        } yield declCode |+| assignCode
      else
        problem("no new variables on left side of :=")(eqPos)
    }
}
