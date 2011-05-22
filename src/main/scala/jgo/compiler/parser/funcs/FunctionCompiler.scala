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

//TODO:  Clean this up.
final class FunctionCompiler(inAtFunc: Input, encl: Scope) extends FuncCompiler with TypeSyntax {
  def scope = encl
  
  private val (funcName, paramsAndResultsM, inAtBrace) = prefix(inAtFunc: Input) match {
    case Success(pos ~ name ~ paramsM ~ resultsM, in1) =>
      val checkForBadVariadic =
        if (badVariadic) Problem("... not permitted except on last param type")(pos)
        else Result(())
      val pAndR = checkForBadVariadic then ((paramsM, resultsM))
      (name: String, pAndR: M[(List[LocalVar], List[LocalVar])], in1)
    
    //case NoSuccess(msg, in) =>
       //("<error>", Problem("syntax error in signature of function definition: %s", msg)(in.pos), in)
  }
  
  lazy val target =
    for ((params, results) <- paramsAndResultsM) yield
      new Function(funcName, FuncType(params map { _.t }, results map { _.t }, variadic))
  
  lazy val inAfter: Input = {
    import lexer._
    
    var cur = inAtBrace
    var nestingLvl = 1 //TODO:  Add logic for syntax errors in signature
    
    while (nestingLvl > 0) {
      cur = cur.rest
      if (cur.first == Keyword("{"))
        nestingLvl += 1
      if (cur.first == Keyword("}"))
        nestingLvl -= 1
    }
    
    cur.rest
  }
  
  lazy val compile: M[FunctionInterm] = 
    paramsAndResultsM flatMap { case (params, results) =>
      val function = target.get
      
      val bodyProc = new Statements {
        def targetFuncType   = function.t
        def hasNamedResults  = namedResults
        def initialEnclosing = encl
        
        //add the params and results to the base scope
        for (param <- params)
          growable.put(param.name, param)
        for (result <- results)
          growable.put(result.name, result)
      }
      
      val codeM = bodyProc.block(inAtBrace) match {
        case bodyProc.Success(codeBuilderM, _) => codeBuilderM map { _.result }
        case bodyProc.NoSuccess(msg, in) => Problem("syntax error in function %s: %s", funcName, msg)(in.pos)
      }
      
      for (code <- codeM) yield
        FunctionInterm(function, params, results, code)
    }
  
  private[this] var variadic, badVariadic = false
  private[this] var namedResults = false
  private[this] var curUnnamed = 0
  
  private[this] def procParamGroup(ids: List[String], vari: Boolean, tM: M[Type]): M[List[LocalVar]] = {
    badVariadic ||= variadic
    variadic = vari
    for (t <- tM) yield
      for (id <- ids) yield
        new LocalVar(id, t)
  }
  
  private[this] def procResultGroup(ids: List[String], tM: M[Type]): M[List[LocalVar]] =
    for (t <- tM) yield
      for (id <- ids) yield
        new LocalVar(id, t)
  
  private[this] val paramsP: PM[List[LocalVar]] =
    "(" ~>! repsep(identList ~ "...".?? ~ goType  ^^ procParamGroup, ",") <~! ")" ^^ { ugly =>
      for (params <- ugly: M[List[List[LocalVar]]]) yield params.flatten
    }
  
  private[this] val unparenResultsP: PM[List[LocalVar]] =
    ( onlyGoType ^^ { _ map { t => curUnnamed += 1; List(new LocalVar("[%d]".format(curUnnamed), t)) } }
    | repsep(onlyGoType, ",")  ^^ { ugly =>
        for (ls <- ugly: M[List[Type]]) yield
          for (t <- ls) yield {
            curUnnamed += 1
            new LocalVar("[%d]".format(curUnnamed), t)
          }
      }
    | repsep(identList ~ goType  ^^ procResultGroup, ",")  ^^ { ugly =>
        namedResults = true
        for (results <- ugly: M[List[List[LocalVar]]]) yield results.flatten
      }
    )
  
  private[this] val resultsP: PM[List[LocalVar]] =
    ( "(" ~> unparenResultsP <~ ")"
    | success(Result(Nil))
    )
  
  private val prefix: Base#P[Pos ~ String ~ M[List[LocalVar]] ~ M[List[LocalVar]]] =
    "func" ~! ident ~! paramsP ~! resultsP
}
