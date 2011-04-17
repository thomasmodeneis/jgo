package jgo.compiler
package interm
package types

import member._

case class StructType(fields: List[FieldDesc]) extends Type {
  val semantics = Value
  
  override val members = {
    var mbrs: Map[String, Member] = Map()
    for (field <- fields) field match {
      case RegularFieldDesc(n, t, tag) =>
        mbrs += (n -> Field(n, t, tag))
      case EmbeddedFieldDesc(n, tn, isP, tag) =>
        val t = if (isP) PointerType(tn) else tn
        mbrs += (n -> Field(n, t, tag))
    }
    mbrs
  }
}

sealed abstract class FieldDesc {
  val name:   String
  val typeOf: Type
  val tag:    Option[String]
}

case class RegularFieldDesc(
  name:   String,
  typeOf: Type,
  tag:    Option[String] = None)
extends FieldDesc

case class EmbeddedFieldDesc(
  name:   String,
  typeOf: Type with Named,
  isPtr:  Boolean,
  tag:    Option[String] = None)
extends FieldDesc
