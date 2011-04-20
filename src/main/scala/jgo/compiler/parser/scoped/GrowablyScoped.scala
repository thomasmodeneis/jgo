package jgo.compiler
package parser
package scoped

import scope._
import interm._
import symbols._

trait GrowablyScoped extends Scoped {
  self: Base =>
  
  def growable: GrowableScope
  
  def bind(name: String, target: Symbol) {
    if (!growable.alreadyDefined(name))
      growable.put(name, target)
    else
      recordErr("symbol `%s' already defined in current scope", name)
  }
}
