package jgo.compiler
package interm
package member

import types._

sealed abstract class Member {
  val typeOf: Type
}

case class Field(name: String, typeOf: Type, tag: Option[String]) extends Member
//case class Method(name: String, typeOf: Type) extends Member(typeOf)

case class EmbeddedMember(f: Field, m: Member) extends Member {
  val typeOf = m.typeOf
}