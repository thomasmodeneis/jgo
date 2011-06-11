package jgo.tools.compiler
package scope

import interm.symbol._

trait EnclosedScope extends Scope {
  val enclosing: Scope
  
  abstract override def get(name: String): Option[Symbol] =
    super.get(name) orElse enclosing.get(name)
  
  abstract override def contains(name: String): Boolean =
    super.contains(name) || enclosing.contains(name)
  
  abstract override def alreadyDefined(name: String): Boolean =
    super.alreadyDefined(name)
  
  /**
   * Returns the ''immediate'' members of this scope, in unspecified order.
   * The members of the enclosing scope are not part of this iterator.
   */
  def iterator: Iterator[Symbol] //NOT ++ enclosing.iterator
  
  override def toString =
    super.toString + "\n" + enclosing.toString
}
