package jgo.tools.compiler
package interm
package types

object Underlying {
  def unapply(t: Type): Some[UnderType] = Some(t.underlying)
}
