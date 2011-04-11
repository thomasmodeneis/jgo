package jgo.compiler
package parser

import scope._

trait GrowablyScoped extends Scoped {
  def growable: GrowableScope
}
