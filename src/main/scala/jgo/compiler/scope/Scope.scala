package jgo.compiler
package scope

import interm.symbols._

import scala.{collection => coll}
import scala.collection.{immutable => imm, mutable => mut, generic => gen}

/*trait =?> [-A, +B] extends PartialFunction[A, B] {
  def andThenPartial [C] (f: PartialFunction[B, C]) = 
    this.lift andThen { _ getOrElse f }
  
  def composePartial [A0] (f: A0 => A) =
    this.lift compose { _ getOrElse f }
}
object =?> {
  implicit def enhancePartial[A, B](f: PartialFunction): A =?> B = new A =?> B {
    def apply(a: A):       B       = f(a)
    def isDefinedAt(a: A): Boolean = f isDefinedAt a
  }
}*/

/*private[symbols] object Utils {
  def pf[Param, Result]
      (apply1: Param => Result)
      (isDefinedAt1: Param => Boolean)
      : PartialFunction[Param, Result] =
    new PartialFunction[Param, Result] {
      def apply       = apply1
      def isDefinedAt = isDefinedAt1
    }
}

import Utils._*/

trait Scope extends PartialFunction[String, Symbol] {
  self =>
  
  def get(name: String):            Option[Symbol]
  def contains(name: String):       Boolean
  def alreadyDefined(name: String): Boolean
  
  final def apply(name: String):       Symbol  = get(name).get
  final def isDefinedAt(name: String): Boolean = contains(name)
  
  /*
  final lazy val valueProj: PartialFunction[String, ValueSymbol] =
    new PartialFunction[String, ValueSymbol] {
      def apply(s: String) = self.apply(s) match {
        case s: ValueSymbol => s
        case _              => throw new NoSuchElementException
      }
      def isDefinedAt(s: String) =
        contains(s) && apply(s).isInstanceOf[ValueSymbol]
    }
  
  final lazy val varProj: PartialFunction[String, VarSymbol] =
    new PartialFunction[String, VarSymbol] {
      def apply(s: String) = self.apply(s) match {
        case s: VarSymbol => s
        case _            => throw new NoSuchElementException
      }
      def isDefinedAt(s: String) =
        contains(s) && apply(s).isInstanceOf[VarSymbol]
    }
  
  final lazy val constProj: PartialFunction[String, ConstSymbol] =
    new PartialFunction[String, ConstSymbol] {
      def apply(s: String) = self.apply(s) match {
        case s: ConstSymbol => s
        case _              => throw new NoSuchElementException
      }
      def isDefinedAt(s: String) =
        contains(s) && apply(s).isInstanceOf[ConstSymbol]
    }
    
  final lazy val typeProj: PartialFunction[String, TypeSymbol] =
    new PartialFunction[String, TypeSymbol] {
      def apply(s: String) = self.apply(s) match {
        case s: TypeSymbol => s
        case _             => throw new NoSuchElementException
      }
      def isDefinedAt(s: String) =
        contains(s) && apply(s).isInstanceOf[TypeSymbol]
    }
  
  final lazy val pkgProj: PartialFunction[String, PkgSymbol] =
    new PartialFunction[String, PkgSymbol] {
      def apply(s: String) = self.apply(s) match {
        case s: PkgSymbol => s
        case _            => throw new NoSuchElementException
      }
      def isDefinedAt(s: String) =
        contains(s) && apply(s).isInstanceOf[PkgSymbol]
    }
  */
}

trait GrowableScope extends Scope {
  def put(name: String, symbol: Symbol): Boolean
}

trait EnclosedScope extends Scope {
  def enclosing: Scope
  
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

object UniverseScope extends Scope {
  private val bindings = imm.Map[String, Symbol]() //fill this in
  
  def get(name: String)            = bindings get name
  def contains(name: String)       = bindings contains name
  def alreadyDefined(name: String) = contains(name)
}
