package jgo.tools.compiler
package scope

import interm.symbol._

import scala.{collection => coll}
import scala.collection.{immutable => imm, mutable => mut, generic => gen}


trait Scope extends PartialFunction[String, Symbol] with coll.Iterable[Symbol] {
  def get(name: String):            Option[Symbol]
  def contains(name: String):       Boolean
  def alreadyDefined(name: String): Boolean
  
  final def apply(name: String):       Symbol  = get(name).get
  final def isDefinedAt(name: String): Boolean = contains(name)
  
  /**
   * Enumerates the ''immediate'' members of this scope; i.e., those members
   * for which [alreadyDefined] would return true.
   */
  def iterator: Iterator[Symbol]
}
