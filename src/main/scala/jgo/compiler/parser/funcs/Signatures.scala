package jgo.compiler
package parser
package funcs

import scoped._
import stmts._
import exprs._
import types._

import scope._
import interm._
import interm.types._
import instr._
import codeseq._
import symbol._

trait Signatures extends TypeSyntax {
  //lazy val signature: PM[Signature]
  
  private lazy val params: PM[(List[ParamVar], Boolean)] =
    ( "(" ~ ")"  ^^^ Result(Nil, false) //shortcut, so to speak
    | namedParams
    | unnamedParams
    )
  
  private lazy val namedParams: PM[(List[LocalVar], Boolean)] =
    "(" ~ repsep(namedParamGroup, ",") <~! ")"  ^^ { case pos ~ ugly =>
      (ugly: M[List[(List[LocalVar], Boolean)]]) flatMap { ls =>
        var variadic, badVariadic = false
        val pss =
          for ((paramGrp, vari) <- ls) yield {
            badVariadic ||= variadic
            variadic = vari
            paramGrp
          }
        val ps = pss.flatten
        if (badVariadic)
          Problem("... not permitted except on last param type")(pos)
        else
          Result((ps, variadic))
      }
    }
  
  private lazy val namedParamGroup: PM[(List[LocalVar], Boolean)] =
    identList ~ "...".?? ~ goType  ^^ { case ids ~ vari ~ tM =>
      for (t <- tM) yield
        if (vari)
          (ids.init.map { id => new LocalVar(id, t) } :+ new LocalVar(ids.last, SliceType(t)), true)
        else
          (ids map { id => new LocalVar(id, t) }, false)
    }
  
  private lazy val unnamedParams: PM[(List[DummyVar], Boolean)] =
    "(" ~ repsep("...".?? ~ goType, ",") <~! ")"  ^^ { case pos ~ ls =>
      var variadic, badVariadic = false
      val psM: M[List[DummyVar]] = //implicit conversion
        for (vari ~ tM <- ls) yield {
          badVariadic ||= variadic
          variadic = vari
          for (t <- tM) yield
            if (vari) new DummyVar(SliceType(t))
            else new DummyVar(t)
        }
      if (badVariadic)
        psM then Problem("... not permitted except on last param type")(pos)
      else
        for (ps <- psM) yield (ps, variadic)
    }
  
  private lazy val namedResults: PM[List[LocalVar]] =
    
  
  
  private lazy val unnamedResults: PM[List[Type]]
}
