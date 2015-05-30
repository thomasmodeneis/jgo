package jgo.tools.compiler
package scope

import interm.symbol._

import scala.{collection => coll}
import scala.collection.{immutable => imm, mutable => mut, generic => gen}


abstract class MapScope extends Scope {
  protected def bindings: coll.Map[String, Symbol]
  
  def get(name: String)                 = bindings get name
  def contains(name: String)            = bindings contains name
  def alreadyDefined(name: String)      = bindings contains name
  
  def iterator = bindings.valuesIterator
  
  override def toString = bindings mkString ("[\n  ", "\n  ", "\n]")
}


class GrowableMapScope extends MapScope with GrowableScope {
  protected val bindings = mut.Map[String, Symbol]()
  
  def put(name: String, symbol: Symbol) = bindings.put(name, symbol) isDefined
}
