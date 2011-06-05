package jgo.compiler
package parser.stmts

import parser.exprs._
import parser.scoped._

import interm._
import expr._
import expr.Combinators
import types._
import symbol._
import codeseq._
import instr._

trait Declarations extends Expressions with GrowablyScoped {
  private var iotaValue = 0
  
  /**
   * Creates a variable of the appropriate kind (i.e., local vs. global) with the specified
   * name and type, returning that variable and any associated declaration code, if appropriate.
   * Subtraits implement this method to specify which of LocalVar or GlobalVar is to be used.
   * In the case of GlobalVar, there would be no declaration code, so an empty code builder
   * is returned instead.
   */
  protected def mkVariable(name: String, typeOf: Type): (Variable, CodeBuilder)
  
  lazy val declaration: Rule[CodeBuilder] =                       "declaration" $
    ( varDecl
    | typeDecl  ^^^ noCode
//  | constDecl
    | failure("not a declaration")
    )
  
  
  lazy val constDecl: P_ =                                  "const declaration" $
    "const" ~>! ( constSpec
                | "(" ~> repWithSemi(constSpec) <~ ")"
                )
  
  lazy val constSpec: P_ =                                    "const decl spec" $
    ( identPosList ~ "=" ~ exprList               //note the first spec in a constDecl may not have form `idList'
    | identPosList ~ goType ~ "=" ~ exprList      //the number of idents must = the number of exprs
    | identPosList                                //don't forget about iota
    )
  
  
  lazy val typeDecl: Rule[Unit] =                            "type declaration" $
    "type" ~>! ( typeSpec
               | "(" ~> repWithSemi(typeSpec) <~ ")"  ^^ foldUnitsTogether
               )
  
  lazy val typeSpec: Rule[Unit] =                              "type decl spec" $
    InPos ~ ident ~ goType  ^^ procTypeSpec
  
  
  lazy val varDecl: Rule[CodeBuilder] =                       "var declaration" $
    "var" ~>! ( varSpec
              | "(" ~> repWithSemi(varSpec) <~ ")"  ^^ foldCodeTogether
              )
  
  lazy val varSpec: Rule[CodeBuilder] =                         "var decl spec" $
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
  
  private def noCode = result(CodeBuilder())
  
  private def foldUnitsTogether(ls: List[Err[Unit]]): Err[Unit] = {
    var res = result(())
    for (action <- ls)
      res = res then action  //Monadic then is awesome!!
    res
  }
  
  private def foldCodeTogether(ls: List[Err[CodeBuilder]]): Err[CodeBuilder] =
    (ls foldLeft result(CodeBuilder.empty)) { (accErr, nextErr) =>
      for ((acc, next) <- (accErr, nextErr))
      yield acc |+| next
    }
  
  
  private def procTypeSpec(pos: Pos, name: String, targetErr: Err[Type]) =
    for (target <- targetErr)
    yield {
      //TypeAlias, since we know that a local type never has any methods of its own
      //TODO: Refine this for structs.
      bind(name, TypeSymbol(new TypeAlias(name, target)))(pos)
      ()
    }
  
  
  private def procVarSpecNoAssign(newVars: List[(String, Pos)], typeOfErr: Err[Type]): Err[CodeBuilder] = 
    typeOfErr flatMap { typeOf =>
      var declCode = CodeBuilder()
      val decledUgly =
        for ((name, pos) <- newVars)
        yield {
          val (v, declC) = mkVariable(name, typeOf)
          declCode = declCode |+| declC
          bind(name, v)(pos)
        }
      Err.liftList(decledUgly) then result(declCode)
    }
  
  private def procVarSpecInfer(left: List[(String, Pos)], eqPos: Pos, rightErr: Err[List[Expr]]): Err[CodeBuilder] =
    rightErr flatMap { right =>
      if (left.length != right.length)
        return problem("arity (%d) of left side of = unequal to arity (%d) of right side",
                       left.length, right.length)(eqPos)
      
      var declCode = CodeBuilder()
      val leftVarsUgly =
        for (((l, pos), r) <- left zip right)
        yield {
          val (v, declC) = mkVariable(l, r.inferenceType)
          declCode = declCode |+| declC
          bind(l, v)(pos)
        }
      for {
        leftVars <- Err.liftList(leftVarsUgly)
        assignCode <- Combinators.assign(leftVars map Combinators.fromVariable, right)(eqPos)
      } yield declCode |+| assignCode
    }
  
  //Fix this function next. //June 5:  ???
  private def procVarSpecTypeAndAssign(left: List[(String, Pos)], typeOfErr: Err[Type], eqPos: Pos, rightErr: Err[List[Expr]]): Err[CodeBuilder] = 
    (typeOfErr, rightErr) flatMap { case (typeOf, right) =>
      var declCode = CodeBuilder()
      val newVarsUgly =
        for ((name, pos) <- left)
        yield {
          val (v, declC) = mkVariable(name, typeOf)
          declCode = declCode |+| declC
          bind(name, v)(pos)
        }
      for {
        newVars <- Err.liftList(newVarsUgly)
        assignCode <- Combinators.assign(newVars map Combinators.fromVariable, right)(eqPos)
      } yield declCode |+| assignCode
    }
}
