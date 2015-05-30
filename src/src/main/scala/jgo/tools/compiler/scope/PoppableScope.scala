package jgo.tools.compiler
package scope

import interm.symbol._

trait PoppableScope[Repr <: PoppableScope[Repr]] extends EnclosedScope {
  val under: Option[Repr]
}

trait PoppableGrowableScope[Repr <: PoppableGrowableScope[Repr]]
  extends PoppableScope[Repr]
  with GrowableScope //LOL!
