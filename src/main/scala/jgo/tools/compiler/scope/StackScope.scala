package jgo.tools.compiler
package scope

import interm.symbol._


sealed trait StackScope extends PoppableGrowableScope[StackScope]
object StackScope {
  def base(encl: Scope) = new GrowableMapScope with StackScope {
    val enclosing = encl
    val under = None
  }
  def frame(encl: StackScope) = new GrowableMapScope with StackScope {
    val enclosing = encl
    val under = Some(encl)
  }
}
