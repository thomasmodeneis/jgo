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
    | parenParams
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
  
  private lazy val parenParams: PM[(List[ParamVar], Boolean)] =
    "(" ~ repsep(namedParamGroup | unnamedParamGroup, ",") <~! ")"  ^^ { case pos ~ ugly =>
      (ugly: M[List[(List[ParamVar], Boolean)]]) flatMap { groups =>
        var variadic, badVariadic, sawNamed, sawUnnamed = false
        val pss =
          for ((paramGrp, vari) <- groups) yield {
            if (paramGrp(0).isInstanceOf[LocalVar])
              sawNamed = true
            else
              sawUnnamed = true
            badVariadic ||= variadic //or: if (variadic) badVariadic = true
            variadic = vari
            paramGrp
          }
        val ps = pss.flatten
        val variErr =
          if (badVariadic) Problem("... not permitted except on last param type")(pos)
          else Result(())
        variErr then {
          if (sawNamed && sawUnnamed)
            Problem("cannot mix named and unnamed parameters")(pos)
          else
            Result((ps, variadic))
        }
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
  
  private lazy val unnamedParamGroup: PM[(List[DummyVar], Boolean)] =
    "...".?? ~ goType  ^^ { case vari ~ tM =>
      for (t <- tM) yield {
        val dummy =
          if (vari) new DummyVar(SliceType(t))
          else new DummyVar(t)
        (List(dummy), vari)
      }
    }
  
  private lazy val parenResults: PM[(List[ParamVar], Boolean)] =
    "(" ~ repsep(namedResultGroup | unnamedResultGroup, ",") <~! ")"  ^^ { case pos ~ ugly =>
      (ugly: M[List[List[ParamVar]]]) flatMap { rss =>
        var sawNamed, sawUnnamed = false
          for (rs <- rss) yield
            if (rs(0).isInstanceOf[LocalVar])
              sawNamed = true
              sawUnnamed = true
        if (sawNamed && sawUnnamed)
          Problem("cannot mix named and unnamed parameters")(pos)
        else
          (rss.flatten, sawNamed)
      }
    }
  
  private lazy val namedResultGroup: PM[List[LocalVar]] =
    identList ~ goType  ^^ { case ids ~ tM =>
      for (t <- tM) yield
        for (id <- ids) yield
          new LocalVar(id, t)
    }
  
  private lazy val unnamedResultGroup: PM[List[DummyVar]] =
    goType ^^ { tM =>
      for (t <- tM) yield
        List(new DummyVar(t))
    }
}
