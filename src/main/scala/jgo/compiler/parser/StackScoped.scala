package jgo.compiler
package parser

import scope._

trait StackScoped extends GrowablyScoped {
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
}
