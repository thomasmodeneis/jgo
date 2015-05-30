package jgo.tools.compiler
package scope

import interm.symbol._

trait GrowableScope extends Scope {
  def put(name: String, symbol: Symbol): Boolean
}
