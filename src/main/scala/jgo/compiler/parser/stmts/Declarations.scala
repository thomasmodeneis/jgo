package jgo.compiler
package parser.stmts

import parser.exprs._
import parser.scoped._

import interm._
import types._
import symbols._
import codeseq._
import instr._

trait Declarations extends Expressions with GrowablyScoped with StmtUtils {
  private var iotaValue = 0
  
  def mkVariable(name: String, typeOf: Type): (Variable, CodeBuilder) = {
    val res = new LocalVar(name, typeOf)
    (res, Decl(res))
  }
  
  lazy val declaration: P[CodeBuilder] =                        "declaration" $
    ( varDecl
    | typeDecl  ^^^ noCode
//  | constDecl
    )
  
  lazy val constDecl: P_ =                                "const declaration" $
    "const" ~>! ( constSpec
                | "(" ~> repWithSemi(constSpec) <~ ")"
                )
  lazy val constSpec: P_ =                                  "const decl spec" $
    ( identList ~ "=" ~ exprList               //note the first spec in a constDecl may not have form `idList'
    | identList ~ goType ~ "=" ~ exprList      //the number of idents must = the number of exprs
    | identList                                //don't forget about iota
    )
  
  lazy val typeDecl: P[Unit] =                            "type declaration" $
    "type" ~>! ( typeSpec
               | "(" ~> repWithSemi(typeSpec) <~ ")"
               )
  lazy val typeSpec: P[Unit] =                               "type decl spec" $
    ident ~ goType  ^^ procTypeSpec
  
  lazy val varDecl: P[CodeBuilder] =                        "var declaration" $
    "var" ~>! ( varSpec
              | "(" ~> repWithSemi(varSpec) <~ ")"  ^^ foldTogether
              )
  lazy val varSpec: P[CodeBuilder] =                          "var decl spec" $
    ( identList          ~ ("=" ~> exprList)  ^^ procVarSpecInfer
    | identList ~ goType ~ ("=" ~> exprList)  ^^ procVarSpecTypeAndAssign
    | identList ~ goType                      ^^ procVarSpecNoAssign
    )
  
  private def iota(): Int = {
    val ret = iotaValue
    iotaValue += 1
    ret
  }
  
  private def resetIota() {
    iotaValue = 0
  }
  
  private def noCode = CodeBuilder()
  
  private def foldTogether(ls: List[CodeBuilder]): CodeBuilder = ls reduceLeft { _ |+| _ }
  
  private def procTypeSpec(name: String, target: Type) {
    bind(name, TypeSymbol(new TypeName(name, target)))
  }
  
  private def procVarSpecInfer(left: List[String], right: List[Expr]): CodeBuilder = {
    var declCode, leftCode, rightCode = CodeBuilder()
    checkArity(left, right)
    for ((l, r) <- left zip right) {
      val (v, dc) = mkVariable(l, r.t)
      bind(l, v)
      declCode  = declCode    |+| dc
      leftCode  = leftCode    |+| r.eval
      rightCode = StoreVar(v) |+| rightCode
    }
    declCode |+| leftCode |+| rightCode
  }
  
  private def procVarSpecTypeAndAssign(left: List[String], typeOf: Type, right: List[Expr]): CodeBuilder = {
    var declCode, leftCode, rightCode = CodeBuilder()
    checkArity(left, right)
    for ((l, r) <- left zip right) {
      if (!(typeOf <<= r.t))
        recordErr("right operand not assignable to specified type %s", typeOf)
      val (v, dc) = mkVariable(l, typeOf)
      bind(l, v)
      declCode  = declCode    |+| dc
      leftCode  = leftCode    |+| r.eval
      rightCode = StoreVar(v) |+| rightCode
    }
    declCode |+| leftCode |+| rightCode
  }
  
  private def procVarSpecNoAssign(newVars: List[String], typeOf: Type): CodeBuilder = {
    var declCode = CodeBuilder()
    for (name <- newVars) {
      val (v, dc) = mkVariable(name, typeOf)
      bind(name, v)
      declCode = declCode |+| dc
    }
    declCode
  }
  
  private implicit def lsUnitP2unitP(p: Parser[List[Unit]]): Parser[Unit] = p ^^^ ()
}
