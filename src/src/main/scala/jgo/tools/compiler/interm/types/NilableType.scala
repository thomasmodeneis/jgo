package jgo.tools.compiler
package interm
package types

trait NilableType extends Type {
  override val isNilable: Boolean = true
}
