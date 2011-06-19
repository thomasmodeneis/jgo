package jgo.tools.compiler
package interm
package symbol

import types._

import util._

sealed abstract class Variable extends ValueSymbol

/*case class PackageVar(pkg: Package, name: String, typeOf: Type) extends Variable(typeOf) {
  override def toString = "var " + pkg.name + "." + name + ", type = " + typeOf
}*/

class GlobalVar(val name: String, val typeOf: Type) extends Variable {
  override def toString = "<global " + name + ": " + typeOf + ">"
  def isPublic: Boolean = !name(0).isLower
}

sealed abstract class ParamVar extends Variable

class LocalVar(val name: String, val typeOf: Type) extends ParamVar with Freezable {
  private var pointedAt: Boolean = false
  
  /**
   * Records that this variable has been closed over or pointed at.
   * 
   * A local variable is said to be ''closed over'' if it is referred
   * to (read from or written to) from within the body of a closure
   * and belongs to the lexical (enclosing) scope of that closure.
   * In other words, a variable is closed over if it is declared
   * outside a certain closure but used inside it.
   * 
   * A local variable is said to be ''pointed at'' if it is the target
   * of an address-of operation, implicit or explicit.
   * 
   * Such variables require special handling at runtime.
   */
  def setPointedAt() {
    errIfFrozen
    pointedAt = true
  }
  
  /**
   * States whether or not this local variable is the target of a pointer
   * or has been referred to from within a closure.
   */
  def isPointedAt: Boolean = pointedAt
  
  override def toString = "<" + name + ": " + typeOf + ">"
}

class DummyVar(val typeOf: Type) extends ParamVar {
  override def toString = "<" + typeOf + ">"
}
