package jgo.tools.compiler
package parser

import parser.exprs.Expressions
import parser.types.Types
import parser.scoped.GrowablyScoped

import interm._
import interm.expr.{Expr, Combinators}
import interm.types._
import symbol._
import codeseq._
import instr._

trait TopLevel extends Expressions with GrowablyScoped {
  //Subclasses override these if they want to do something
  //with this info.
  protected def registerTypeDecl(name: String, t: WrappedType) { }
  protected def registerVarDecl(name: String, v: GlobalVar) { }
  protected def addInitCode(code: CodeBuilder) { }
  
  
  private var iotaValue = 0
  
  
  lazy val declaration: Rule[Unit] =                              "declaration" $
    ( typeDecl
//  | constDecl
    | varDecl
    | failure("not a declaration")
    )
  
  
  
  lazy val typeDecl: Rule[Unit] =                            "type declaration" $
    "type" ~>! ( typeSpec
               | "(" ~> repWithSemi(typeSpec) <~ ")"  ^^ foldUnitsTogether
               )
  
  lazy val typeSpec: Rule[Unit] =                              "type decl spec" $
    InPos ~ ident ~ goType  ^^ procTypeSpec
  
  
  
  lazy val constDecl: P_ =                                  "const declaration" $
    "const" ~>! ( constSpec
                | "(" ~> repWithSemi(constSpec) <~ ")"
                )
  
  lazy val constSpec: P_ =                                    "const decl spec" $
    ( identPosList ~ "=" ~ exprList           //the first spec in a constDecl may not have form `idList'
    | identPosList ~ goType ~ "=" ~ exprList  //the number of idents must = the number of exprs
    | identPosList                            //don't forget about iota
    )
  
  
  
  lazy val varDecl: Rule[Unit] =                              "var declaration" $
    "var" ~>! ( varSpec
              | "(" ~> repWithSemi(varSpec) <~ ")"  ^^ foldUnitsTogether
              )
  
  lazy val varSpec: Rule[Unit] =                                "var decl spec" $
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
  
  
  private def foldUnitsTogether(ls: List[Err[Unit]]): Err[Unit] = {
    var res = result(())
    for (action <- ls)
      res = res then action  //Monadic then is awesome!!
    res
  }
  
  
  private def procTypeSpec(pos: Pos, name: String, targetErr: Err[Type]) =
    targetErr flatMap { target =>
      //All types declared at top-level must be wrapped types, not type aliases,
      //since we cannot guarantee that they will never have methods.
      //We could make all types that don't have methods type aliases, but then
      //adding a method to such a type would be a binary-incompatible change.
      //Besides, the second method is more complicated and yields inappropriate
      //results for structs, since I assume that users would like their top level
      //struct types to correspond to Java classes, whether or not they have methods.
      bind(name, TypeSymbol(new WrappedType(name, target)))(pos) withResult ()
    }
  
  
  private def procVarSpecNoAssign(newVars: List[(String, Pos)], typeOfErr: Err[Type]): Err[Unit] = 
    typeOfErr flatMap { typeOf =>
      val decledUgly =
        for ((name, pos) <- newVars)
        yield {
          val v = new GlobalVar(name, typeOf)
          bind(name, v)(pos)
        }
      Err.liftList(decledUgly) withResult ()
    }
  
  
  private def procVarSpecInfer(left:     List[(String, Pos)],
                               eqPos:    Pos,
                               rightErr: Err[List[Expr]]): Err[Unit] =
    rightErr flatMap { right =>
      if (left.length != right.length)
        return problem("arity (%d) of left side of = unequal to arity (%d) of right side",
                       left.length, right.length)(eqPos)
      
      val leftVarsUgly =
        for (((l, pos), r) <- left zip right)
        yield {
          val v = new GlobalVar(l, r.inferenceType)
          bind(l, v)(pos)
        }
      for {
        leftVars <- Err.liftList(leftVarsUgly)
        assignCode <- Combinators.assign(leftVars map Combinators.fromVariable, right)(eqPos)
      } yield addInitCode(assignCode)
    }
  
  
  private def procVarSpecTypeAndAssign(left:      List[(String, Pos)],
                                       typeOfErr: Err[Type],
                                       eqPos:     Pos,
                                       rightErr:  Err[List[Expr]]): Err[Unit] = 
    (typeOfErr, rightErr) flatMap { case (typeOf, right) =>
      val newVarsUgly =
        for ((name, pos) <- left)
        yield {
          val v = new GlobalVar(name, typeOf)
          bind(name, v)(pos)
        }
      for {
        newVars <- Err.liftList(newVarsUgly)
        assignCode <- Combinators.assign(newVars map Combinators.fromVariable, right)(eqPos)
      } yield addInitCode(assignCode)
    }
}
