package jgo.compiler
package parser.stmts

import parser.Base

import interm._
import types._
import symbol._
import codeseq._
import instr._

import scala.collection.mutable.{HashSet, HashMap}

/**
 * Provides an implementation of the semantics of breaks and continues,
 * and of constructs that can be broken out of or continued.
 * 
 * This implementation uses mutable state, maintaining a stack of breakable
 * constructs and of continuable constructs (for targetless breaks and continues)
 * and hashes containing currently valid break- and continue-targets (for
 * breaks and continues that specify a target label).
 * 
 * The public interface of this trait consists of semantic p-combinators for
 * unlabeled and labeled breakables and loops (constructs that support both
 * breaks and continues) and accessors which provide M-wrapped intermediate
 * code for breaking out of or continuing the innermost (valid) enclosing
 * construct or the the construct labeled with the specified name.
 */
trait BreaksAndContinues {
  self: Base =>
  
  private var breakStack, continueStack = List[Label]()
  private val validBreaks, validContinues = HashMap[String, Label]()
  
  def breakable[T](p: Parser[M[Label => T]]): Parser[M[T]] = Parser { in =>
    val breakTarget = new Label("break")
    
    breakStack ::= breakTarget
    val res = p(in) map { fM => fM(breakTarget) }
    breakStack = breakStack.tail
    res
  }
  
  def loop[T](p: Parser[M[(Label, Label) => T]]): Parser[M[T]] = Parser { in =>
    val g = new LabelGroup
    val breakTarget    = new Label("break", g)
    val continueTarget = new Label("continue", g)
    
    breakStack    ::= breakTarget
    continueStack ::= continueTarget
    val res = p(in) map { fM => fM(breakTarget, continueTarget) }
    continueStack = continueStack.tail
    breakStack    = breakStack.tail
    res
  }
  
  def labeledBreakable(p: Parser[M[Label => CodeBuilder]])(tuple: (String, M[UserLabel])): Parser[M[CodeBuilder]] = {
    val (name, lblM) = tuple
    
    Parser { in =>
      val g = new LabelGroup(name)
      val breakTarget = new Label("break", g)
      
      breakStack ::= breakTarget
      validBreaks(name) = breakTarget
      
      val parseResult = p(in) map { fM => fM(breakTarget) }
      
      validBreaks -= name
      breakStack = breakStack.tail
      
      for (resultM <- parseResult)
      yield for ((lbl, result) <- (lblM, resultM))
      yield Lbl(lbl) |+| result
    }
  }
  
  def labeledLoop(p: Parser[M[(Label, Label) => CodeBuilder]])(tuple: (String, M[UserLabel])): Parser[M[CodeBuilder]] = {
    val (name, lblM) = tuple
    
    Parser { in =>
      val g = new LabelGroup(name)
      val breakTarget = new Label("break", g)
      val continueTarget = new Label("continue", g)
      
      breakStack    ::= breakTarget
      continueStack ::= continueTarget
      validBreaks(name)    = breakTarget
      validContinues(name) = continueTarget
      
      val parseResult = p(in) map { fM => fM(breakTarget, continueTarget) }
      
      validContinues -= name
      validBreaks    -= name
      continueStack = continueStack.tail
      breakStack    = breakStack.tail
      
      for (resultM <- parseResult)
      yield for ((lbl, result) <- (lblM, resultM))
      yield Lbl(lbl) |+| result
    }
  }
  
  def procBreak(pos: Pos): M[CodeBuilder] = breakStack.headOption match {
    case Some(target) => Result(Goto(target))
    case None => Problem("illegal break; not enclosed by loop, switch, or select")(pos)
  }
  
  def procContinue(pos: Pos): M[CodeBuilder] = continueStack.headOption match {
    case Some(target) => Result(Goto(target))
    case None => Problem("illegal continue; not enclosed by loop")(pos)
  }
  
  def procBreak(pos: Pos, name: String): M[CodeBuilder] = validBreaks get name match {
    case Some(target) => Result(Goto(target))
    case None => Problem("illegal break target %s; not an enclosing loop, switch, or select", name)(pos)
  }
  
  def procContinue(pos: Pos, name: String): M[CodeBuilder] = validContinues get name match {
    case Some(target) => Result(Goto(target))
    case None => Problem("illegal continue target %s; not an enclosing loop", name)(pos)
  }
}
