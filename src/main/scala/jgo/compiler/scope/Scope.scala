package jgo.compiler
package scope

import interm.symbols._

import scala.{collection => coll}
import scala.collection.{immutable => imm, mutable => mut, generic => gen}


trait Scope extends PartialFunction[String, Symbol] {
  def get(name: String):            Option[Symbol]
  def contains(name: String):       Boolean
  def alreadyDefined(name: String): Boolean
  
  final def apply(name: String):       Symbol  = get(name).get
  final def isDefinedAt(name: String): Boolean = contains(name)
}

trait GrowableScope extends Scope {
  def put(name: String, symbol: Symbol): Boolean
}

trait EnclosedScope extends Scope {
  val enclosing: Scope
  
  abstract override def get(name: String): Option[Symbol] =
    super.get(name) orElse enclosing.get(name)
  
  abstract override def contains(name: String): Boolean =
    super.contains(name) || enclosing.contains(name)
  
  abstract override def alreadyDefined(name: String): Boolean =
    super.alreadyDefined(name)
}

class MapScope private (bindings: mut.Map[String, Symbol]) extends GrowableScope {
  def this() = this(mut.Map[String, Symbol]())
  
  def get(name: String)                 = bindings get name
  def contains(name: String)            = bindings contains name
  def alreadyDefined(name: String)      = bindings contains name
  def put(name: String, symbol: Symbol) = bindings.put(name, symbol) isDefined
}

class SequentialScope(val enclosing: Scope) extends MapScope with EnclosedScope
