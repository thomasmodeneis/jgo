package jgo.compiler
package scope

import interm.symbols._

import scala.{collection => coll}
import scala.collection.{immutable => imm, mutable => mut, generic => gen}


trait Scope extends PartialFunction[String, Symbol] with coll.Iterable[Symbol] {
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

trait PoppableScope[Repr <: PoppableScope[Repr]] extends EnclosedScope {
  val under: Option[Repr]
}

trait PoppableGrowableScope[Repr <: PoppableGrowableScope[Repr]] extends PoppableScope[Repr]
                                                                    with GrowableScope //LOL!

abstract class MapScope extends Scope {
  protected val bindings: coll.Map[String, Symbol]
  
  def get(name: String)                 = bindings get name
  def contains(name: String)            = bindings contains name
  def alreadyDefined(name: String)      = bindings contains name
  
  def iterator = bindings.valuesIterator
}

class GrowableMapScope private (protected val bindings: mut.Map[String, Symbol]) extends MapScope with GrowableScope {
  def this() = this(mut.Map[String, Symbol]())
  
  def put(name: String, symbol: Symbol) = bindings.put(name, symbol) isDefined
}

sealed trait SequentialScope extends PoppableGrowableScope[SequentialScope]
object SequentialScope {
  def base(encl: Scope) = new GrowableMapScope with SequentialScope {
    val enclosing = encl
    val under = None
  }
  def frame(encl: SequentialScope) = new GrowableMapScope with SequentialScope {
    val enclosing = encl
    val under = Some(encl)
  }
}
