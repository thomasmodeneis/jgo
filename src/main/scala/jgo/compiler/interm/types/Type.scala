package jgo.compiler
package interm
package types

import member._

/**
 * A static type in the Go programming language.
 */
trait Type extends Membered {
  
  /**
   * The underlying type of this type.
   * The ''underlying type'' of a type ''T'' is defined as follows:
   * - If ''T'' is a named type, then the referent of that named type;
   * - Otherwise, ''T'' itself.
   * In the latter case, we call ''T'' an ''under-type''.
   * Based on these rules, it is guaranteed that the underlying type
   * of any type is an under-type.
   */
  def underlying: UnderType
  
  /**
   * The actual statically-known type of a value of this type at runtime.
   * Such type is said to be this type's ''effective type''.
   * Except for type aliases (named types that are guaranteed never to
   * have any methods of their own), every type has itself as its
   * effective type.
   * 
   * @todo make above definition more clear
   */
  def effective: Type
  
  /**
   * Indicates whether this type is an interface type or pointer thereto.
   */
  def isInterface: Boolean = false //default to false
  
  /**
   * Indicates whether `nil` is considered a value of this type, and may
   * therefore be the target of an assignment of a variable of this type.
   */
  def isNilable: Boolean = //overridden by NilableType for "efficiency"
    underlying.isInstanceOf[NilableType]
  
  def semantics: Semantics
  
  val members: Map[String, Member] = Map()
  
  /**
   * States whether values of this type and values of the specified type are
   * comparable. Two types are said to have comparable values (colloquially,
   * two types are said to be comparable) if values of either type are
   * assignable to variables of the other (colloquially, if one type is
   * assignable to the other).
   * 
   * @todo Refine comparability to better abide by the spec.
   */
  final def <=> (other: Type): Boolean =
    Type.canHold(this, other) || Type.canHold(other, this)
  
  /**
   * States whether values of the specified type are assignable to variables
   * of this type.
   */
  final def <<= (other: Type): Boolean = Type.canHold(this, other)
  
  /**
   * States whether values of this type are assignable to variables of
   * the specified type.
   */
  final def =>> (other: Type): Boolean = Type.canHold(other, this)
}

/**
 * Fundamental operations on types.
 */
object Type {
  import PartialFunction._
  /**
   * Indicates whether the two given types are identical to each other.
   * Through the miracle of case classes, we get this property for free.
   */
  def identical(t1: Type, t2: Type): Boolean =
    t1 == t2
  
  /**
   * Indicates whether a variable of the first type ''can hold''
   * a value of the second type; that is, whether the second type
   * is assignable to the first.
   * 
   * @todo verify conformity with the spec
   */
  def canHold(t1: Type, t2: Type): Boolean = {
    identical(t1, t2) ||
    (
      !( //not both named types
        t1.isInstanceOf[NamedType] &&
        t2.isInstanceOf[NamedType]
      ) && (
        identical(t1.underlying, t2.underlying) ||
        cond((t1.underlying, t2.underlying)) {
          case (ChanType(elemT1, _, _), BidirChanType(elemT2)) => identical(elemT1, elemT2)
        }
      )
    ) ||
    cond((t1, t2)) {
      case (TopType, _)                => true
      case (n, NilType) if n.isNilable => true
      case (_, BottomType)             => true
      
      case (t, u: UntypedConstType)    => u.canFitIn(t.underlying)
      
      //interface logic goes here
    }
  }
}

sealed abstract class Semantics
  case object Value     extends Semantics
  case object Reference extends Semantics
  case object Primitive extends Semantics
