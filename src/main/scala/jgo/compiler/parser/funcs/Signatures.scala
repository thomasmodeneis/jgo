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

import Signature._

trait Signatures extends Base {
  self: Types =>
  
  lazy val signature: Rule[Signature] =
    ( params ~ parenResults  ^^ procParenResultSig
    | params ~ goType        ^^ procOneResultSig
    | params                 ^^ procVoidSig
    )
  
  private def procParenResultSig(psErr: Err[(List[ParamVar], Boolean)], rsErr: Err[(List[ParamVar], Boolean)]) =
    for (((ps, vari), (rs, namedRes)) <- (psErr, rsErr))
    yield results(ps, rs, vari, namedRes)
  
  private def procOneResultSig(psErr: Err[(List[ParamVar], Boolean)], tErr: Err[Type]) =
    for (((ps, vari), t) <- (psErr, tErr))
    yield singleResult(ps, t, vari)
  
  private def procVoidSig(psErr: Err[(List[ParamVar], Boolean)]) =
    for ((ps, vari) <- psErr)
    yield noResults(ps, vari)
  
  private lazy val params: Rule[(List[ParamVar], Boolean)] =
    ( "(" ~ ")"  ^^^ result(Nil, false) //shortcut, so to speak
    | parenParams
    )
  
  private lazy val namedParams: Rule[(List[LocalVar], Boolean)] =
    "(" ~ repsep(namedParamGroup, ",") <~! ")"  ^^ { case pos ~ ugly =>
      Err.liftList(ugly) flatMap { ls =>
        var variadic, badVariadic = false
        val pss =
          for ((paramGrp, vari) <- ls)
          yield {
            badVariadic ||= variadic
            variadic = vari
            paramGrp
          }
        val ps = pss.flatten
        if (badVariadic)
          problem("... not permitted except on last param type")(pos)
        else
          result((ps, variadic))
      }
    }
  
  private lazy val parenParams: Rule[(List[ParamVar], Boolean)] =
    "(" ~ repsep(namedParamGroup | unnamedParamGroup, ",") <~! ")"  ^^ { case pos ~ ugly =>
      Err.liftList(ugly) flatMap { groups =>
        var variadic, badVariadic, sawNamed, sawUnnamed = false
        val pss =
          for ((paramGrp, vari) <- groups)
          yield {
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
          if (badVariadic) problem("... not permitted except on last param type")(pos)
          else result(())
        variErr then {
          if (sawNamed && sawUnnamed)
            problem("cannot mix named and unnamed parameters")(pos)
          else
            result((ps, variadic))
        }
      }
    }
  
  private lazy val namedParamGroup: Rule[(List[LocalVar], Boolean)] =
    identList ~ "...".?? ~ goType  ^^ { case ids ~ vari ~ tErr =>
      for (t <- tErr)
      yield
        if (vari)
          (ids.init.map { id => new LocalVar(id, t) } :+ new LocalVar(ids.last, SliceType(t)), true)
        else
          (ids map { id => new LocalVar(id, t) }, false)
    }
  
  private lazy val unnamedParamGroup: Rule[(List[DummyVar], Boolean)] =
    "...".?? ~ goType  ^^ { case vari ~ tErr =>
      for (t <- tErr)
      yield {
        val dummy =
          if (vari) new DummyVar(SliceType(t))
          else new DummyVar(t)
        (List(dummy), vari)
      }
    }
  
  private lazy val parenResults: Rule[(List[ParamVar], Boolean)] =
    "(" ~ repsep(namedResultGroup | unnamedResultGroup, ",") <~! ")"  ^^ { case pos ~ ugly =>
      (ugly: Err[List[List[ParamVar]]]) flatMap { rss =>
        var sawNamed, sawUnnamed = false
          for (rs <- rss)
          yield
            if (rs(0).isInstanceOf[LocalVar])
              sawNamed = true
            else
              sawUnnamed = true
        if (sawNamed && sawUnnamed)
          problem("cannot mix named and unnamed parameters")(pos)
        else
          result((rss.flatten, sawNamed))
      }
    }
  
  private lazy val namedResultGroup: Rule[List[LocalVar]] =
    identList ~ goType  ^^ { case ids ~ tErr =>
      for (t <- tErr)
      yield for (id <- ids)
      yield new LocalVar(id, t)
    }
  
  private lazy val unnamedResultGroup: Rule[List[DummyVar]] =
    goType ^^ { tErr =>
      for (t <- tErr)
      yield List(new DummyVar(t))
    }
}
