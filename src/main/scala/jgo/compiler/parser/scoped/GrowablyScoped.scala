package jgo.compiler
package parser
package scoped

import scope._
import interm._
import symbol._

trait GrowablyScoped extends Scoped {
  self: Base =>
  
  def growable: GrowableScope
  
  def bind[S <: Symbol](name: String, target: S)(implicit pos: Pos): M[S] =
    if (!growable.alreadyDefined(name)) {
      growable.put(name, target)
      Result(target)
    }
    else
      Problem("symbol `%s' already defined in current scope", name)
}
