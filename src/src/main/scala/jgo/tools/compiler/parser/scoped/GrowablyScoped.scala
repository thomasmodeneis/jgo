package jgo.tools.compiler
package parser
package scoped

import scope._
import interm._
import symbol._

trait GrowablyScoped extends Scoped {
  self: Base =>
  
  protected def growable: GrowableScope
  
  //I may change this to have result type Err[Unit] instead of Err[S]
  //since the latter is sort of non-cohesive and since it appears that I
  //always follow a call to bind with a `withResult ()` or equivalent.
  protected def bind[S <: Symbol](name: String, target: S)(implicit pos: Pos): Err[S] =
    if (!growable.alreadyDefined(name)) {
      growable.put(name, target)
      result(target)
    }
    else
      problem("symbol `%s' already defined in current scope", name)
}
