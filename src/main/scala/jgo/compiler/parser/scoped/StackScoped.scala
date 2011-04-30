package jgo.compiler
package parser
package scoped

import scope._

import interm._
import codeseq._
import instr.Undecl
import symbol.LocalVar

trait StackScoped extends GrowablyScoped {
  self: Base =>
  
  //def, not val, because val leads to NPE
  //if val, will be null until subclass constructor call
  //which happens after curScope is initialized.
  //So curScope would be initialized to SeqScope.base(null)
  protected def initialEnclosing: Scope
  
  private var curScope: SequentialScope = SequentialScope.base(initialEnclosing)
  
  def scope = curScope
  def growable = curScope
  
  def push() {
    curScope = SequentialScope.frame(curScope)
  }
  def pop() {
    curScope = curScope.under getOrElse (throw new IllegalStateException)
  }
  
  def undecl(): CodeBuilder = {
    var code = CodeBuilder()
    for (s <- curScope) s match {
      case l: LocalVar =>
        l.freeze
        code = code |+| Undecl(l)
    }
    code
  }
  
  
  def scoped[T](p: Parser[T]): Parser[T ~ CodeBuilder] = Parser {
    in =>
    push()
    val res = for (r <- p(in)) yield new ~(r, undecl())
    pop()
    res
  }
}
