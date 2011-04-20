package jgo.compiler
package parser

import scope._

import interm._
import codeseq._
import instr.Undecl
import symbols.LocalVar

trait StackScoped extends GrowablyScoped {
  self: Base =>
  
  protected val initialEnclosing: Scope
  
  private var curScope: SequentialScope = SequentialScope.base(initialEnclosing)
  
  def scope = curScope
  def growable = curScope
  
  def push() {
    curScope = SequentialScope.frame(curScope)
  }
  def pop() {
    curScope = curScope.tail getOrElse (throw new IllegalStateException)
  }
  
  def undecl(): CodeBuilder {
    //implement me!!!
  }
  
  
  def scoped[T](p: Parser[T]): Parser[T ~ CodeBuilder] = Parser {
    in =>
    push()
    val res = for (r <- p(in)) yield new ~(r, undecl())
    pop()
    res
  }
}
