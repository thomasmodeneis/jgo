package jgo.tools.compiler
package interm

import types._

sealed abstract class Member extends Typed {
  def enclosing: Type
  def isMethod: Boolean
  def depth: Int
}

sealed abstract class BasicMember extends Member {
  def depth = 0
}

case class FieldMember(enclosing: Type, name: String, typeOf: Type) extends BasicMember {
  def isMethod = false
}

case class MethodMember(enclosing: Type, name: String, typeOf: FuncType) extends BasicMember {
  def isMethod = true;
}

case class EmbeddedMember(f: FieldMember, m: Member) extends Member {
  val typeOf = m.typeOf
  def enclosing = f.enclosing
  val isMethod = m.isMethod
  val depth = m.depth + 1
}

case class WrappedMember(wrappedEncl: Type, m: Member) extends Member {
  val typeOf = m.typeOf
  def enclosing = wrappedEncl
  val isMethod = m.isMethod
  val depth = m.depth
}
