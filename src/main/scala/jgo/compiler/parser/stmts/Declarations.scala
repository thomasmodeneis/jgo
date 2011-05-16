package jgo.compiler
package parser.stmts

import parser.exprs._
import parser.scoped._

import interm._
import expr._
import expr.{Combinators => C}
import types._
import symbol._
import codeseq._
import instr._

trait Declarations extends Expressions with GrowablyScoped with StmtUtils {
  private var iotaValue = 0
  
  /**
   * Creates a variable of the appropriate kind (i.e., local vs. global) with the specified
   * name and type, returning that variable and any associated declaration code, if appropriate.
   * Subtraits implement this method to specify which of LocalVar or GlobalVar is to be used.
   * In the case of GlobalVar, there would be no declaration code, so an empty code builder
   * is returned instead.
   */
  protected def mkVariable(name: String, typeOf: Type): (Variable, CodeBuilder)
  
  lazy val declaration: PM[CodeBuilder] =                       "declaration" $
    ( varDecl
    | typeDecl  ^^^ noCode
//  | constDecl
    | failure("not a declaration")
    )
  
  lazy val constDecl: P_ =                                "const declaration" $
    "const" ~>! ( constSpec
                | "(" ~> repWithSemi(constSpec) <~ ")"
                )
  lazy val constSpec: P_ =                                  "const decl spec" $
    ( identPosList ~ "=" ~ exprList               //note the first spec in a constDecl may not have form `idList'
    | identPosList ~ goType ~ "=" ~ exprList      //the number of idents must = the number of exprs
    | identPosList                                //don't forget about iota
    )
  
  lazy val typeDecl: PM[Unit] =                            "type declaration" $
    "type" ~>! ( typeSpec
               | "(" ~> repWithSemi(typeSpec) <~ ")"  ^^ foldUnitsTogether
               )
  lazy val typeSpec: PM[Unit] =                              "type decl spec" $
    InPos ~ ident ~ goType  ^^ procTypeSpec
  
  lazy val varDecl: PM[CodeBuilder] =                       "var declaration" $
    "var" ~>! ( varSpec
              | "(" ~> repWithSemi(varSpec) <~ ")"  ^^ foldCodeTogether
              )
  lazy val varSpec: PM[CodeBuilder] =                         "var decl spec" $
    ( identPosList          ~ pos("=") ~ exprList  ^^ procVarSpecInfer
    | identPosList ~ goType ~ pos("=") ~ exprList  ^^ procVarSpecTypeAndAssign
    | identPosList ~ goType                        ^^ procVarSpecNoAssign
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
    var res = M(())
    for (action <- ls)
      res = res then action  //Monadic then is awesome!!
    res
  }
  
  private def foldCodeTogether(ls: List[M[CodeBuilder]]): M[CodeBuilder] =
    (ls foldLeft M(CodeBuilder.empty)) { (accM, nextM) =>
      for ((acc, next) <- (accM, nextM))
      yield acc |+| next
    }
  
  
  private def procTypeSpec(pos: Pos, name: String, targetM: M[Type]) =
    Result(()) after {
      for (target <- targetM)
      yield bind(name, TypeSymbol(new WrappedType(name, target)))(pos)
    }
  
  
  private def procVarSpecNoAssign(newVars: List[(String, Pos)], typeOfM: M[Type]): M[CodeBuilder] = 
    typeOfM flatMap { typeOf =>
      var declCode = CodeBuilder()
      val decled =
        for ((name, pos) <- newVars)
        yield {
          val (v, declC) = mkVariable(name, typeOf)
          declCode = declCode |+| declC
          bind(name, v)(pos)
        }
      decled then declCode //implicitly convert List[M[Variable]] -> M[List[Variable]]
    }
  
  private def procVarSpecInfer(left: List[(String, Pos)], eqPos: Pos, rightM: M[List[Expr]]): M[CodeBuilder] =
    rightM flatMap { right =>
      checkArity(left, right) then {
        var declCode = CodeBuilder()
        val leftVarsM: M[List[Variable]] =
          for (((l, pos), r) <- left zip right)
          yield {
            val (v, declC) = mkVariable(l, r.t)
            declCode = declCode |+| declC
            bind(l, v)(pos)
          } //implicit conv
        for {
          leftVars <- leftVarsM
          assignCode <- C.assign(leftVars map varLval, right)(eqPos)
        } yield declCode |+| assignCode
      }
    }
  
  //Fix this function next.
  private def procVarSpecTypeAndAssign(left: List[(String, Pos)], typeOfM: M[Type], eqPos: Pos, rightM: M[List[Expr]]): M[CodeBuilder] = 
    (typeOfM, rightM) flatMap { case (typeOf, right) =>
      var declCode = CodeBuilder()
      val newVarsM: M[List[Variable]] = //implicit conv
        for ((name, pos) <- left)
        yield {
          val (v, declC) = mkVariable(name, typeOf)
          declCode = declCode |+| declC
          bind(name, v)(pos)
        }
      for {
        newVars <- newVarsM
        assignCode <- C.assign(newVars map varLval, right)(eqPos)
      } yield declCode |+| assignCode
    }
}
