package jgo.compiler
package parser.stmts

import message._

import parser.exprs._
import parser.scoped._

//import interm._
import interm.expr._
import interm.types._
import interm.symbol._
import interm.codeseq._
import interm.instr._

trait Declarations extends Expressions with GrowablyScoped with StmtUtils {
  private var iotaValue = 0
  
  protected def mkVariable(name: String, typeOf: Type): (Variable, CodeBuilder) = {
    val res = new LocalVar(name, typeOf)
    (res, Decl(res))
  }
  
  lazy val declaration: PM[CodeBuilder] =                       "declaration" $
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
  
  lazy val typeDecl: PM[Unit] =                            "type declaration" $
    "type" ~>! ( typeSpec
               | "(" ~> repWithSemi(typeSpec) <~ ")"  ^^ foldUnitsTogether
               )
  lazy val typeSpec: PM[Unit] =                              "type decl spec" $
    ident ~ goType  ^^ procTypeSpec
  
  lazy val varDecl: PM[CodeBuilder] =                       "var declaration" $
    "var" ~>! ( varSpec
              | "(" ~> repWithSemi(varSpec) <~ ")"  ^^ foldCodeTogether
              )
  lazy val varSpec: PM[CodeBuilder] =                         "var decl spec" $
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
  
  private def noCode = Result(CodeBuilder())
  
  private def foldUnitsTogether(ls: List[M[Unit]]): M[Unit] = {
    var res = Result(())
    for (action <- ls)
      res = res then action  //Monadic then is awesome!!
    res
  }
  
  private def foldCodeTogether(ls: List[M[CodeBuilder]]): M[CodeBuilder] =
    (ls foldLeft Result(CodeBuilder())) { (accM, nextM) =>
      for ((acc, next) <- together2(accM, nextM))
      yield acc |+| next
    }
  
  private def procTypeSpec(name: String, targetM: M[Type]) = {
    for (target <- targetM)
      bind(name, TypeSymbol(new WrappedType(name, target))) //I'm pretty sure this gets eval'd "now"
  }
  
  private def procVarSpecInfer(left: List[String], rightM: M[List[Expr]]) =
    for (right <- rightM)
    yield for ((l, r) <- left zip right) {
      val (v, dc) = mkVariable(l, r.t)
      bind(l, v)
    }
  
  private def procVarSpecTypeAndAssign(left: List[String], typeOfM: M[Type], rightM: M[List[Expr]]) = 
    for ((typeOf, right) <- together2(typeOfM, rightM))
    yield {
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
