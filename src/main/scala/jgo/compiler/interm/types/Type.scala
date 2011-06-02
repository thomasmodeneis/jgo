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
   */
  def canHold(t1: Type, t2: Type): Boolean =
    identical(t1, t2) ||
    (
      !t1.isInstanceOf[NamedType] &&
      !t2.isInstanceOf[NamedType] && {
      (t1, t2) match {
        case (TopType, _)                => true
        case (n, NilType) if n.isNilable => true
        case (_, BottomType)             => true
        
        case (t, u: UntypedConstType)
          if u.canFitIn(t.underlying)    => println("saw untyped"); true
        
        //interface logic goes here
        
        /* 
          ALERT: These upcoming variance rules are not in the spec!
          The spec makes these things invariant.  I want to see if they
          work with the, IMHO, more cohesive and traditional variances below.
        */
        
        //receiving channels are covariant in their element types
        case (ChanType(elem1, true, false), ChanType(elem2, true, _))
          if canHold(elem1, elem2)       => true
        
        //sending channels are contravariant in their element types
        case (ChanType(elem1, false, true), ChanType(elem2, _, true))
          if canHold(elem2, elem1)       => true
        
        //function types are covariant in results and contravariant in parameters
        case (FuncType(ps1, rs1, vadic1), FuncType(ps2, rs2, vadic2)) if (
          vadic1 == vadic2 &&
          ps1.zipAll(ps2, TopType, BottomType).forall {
            case (param1, param2) =>    param1 =>> param2
          } &&
          rs1.zipAll(rs2, BottomType, TopType).forall {
            case (reslt1, reslt2) =>    reslt1 <<= reslt2
          }
        )                                => true
        
        case _                           => false
      } }
    ) ||
    identical(t1.underlying, t2.underlying) && !( //not both
      t1.isInstanceOf[NamedType] &&
      t2.isInstanceOf[NamedType]
    )
}

sealed abstract class Semantics
  case object Value     extends Semantics
  case object Reference extends Semantics
  case object Primitive extends Semantics
