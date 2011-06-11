package jgo.tools.compiler
package parser
package scoped

import scope._
import interm._
import symbol._

trait GrowablyScoped extends Scoped {
  self: Base =>
  
  protected def growable: GrowableScope
  
  protected def bind[S <: Symbol](name: String, target: S)(implicit pos: Pos): Err[S] =
    if (!growable.alreadyDefined(name)) {
      growable.put(name, target)
      result(target)
    }
    else
      problem("symbol `%s' already defined in current scope", name)
}
