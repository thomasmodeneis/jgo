package jgo.compiler
package interm
package types

trait UnderType extends Type {
  def effective: Type = this
  def underlying: UnderType = this
}
