package jgo.compiler
package interm
package types

trait BuiltinType extends Type with Constable

trait PrimitiveType extends BuiltinType {
  val semantics = Primitive
}
