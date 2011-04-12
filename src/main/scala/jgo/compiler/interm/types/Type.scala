package jgo.compiler
package interm
package types

import member._

trait Type extends Equals with Membered {
  /**
   * Indicates whether this type is an interface type or pointer thereto.
   */
  val isInterface: Boolean = false //default to false
  
  val semantics: Semantics
  
  val underlying: Type = this //overridden by TypeName
  
  val nilable: Boolean = false //overridden by Nilable, TypeName
  
  val members = Map()
  
  /**
   * States whether or not this type is identical to the given type,
   * behaving just like a call to ==.
   */
  //final def === (other: Type): Boolean =
  //  this == other
  
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

object Type {
  def canHold(t1: Type, t2: Type): Boolean =
    t1 == t2 ||
    (
      !t1.isInstanceOf[TypeName] &&
      !t2.isInstanceOf[TypeName] && {
      (t1, t2) match {
        case (TopType, _)              => true
        case (n, NilType) if n.nilable => true
        case (_, BottomType)           => true
        //interface logic goes here
        //receiving channels are covariant in their element types
        case (ChanType(elem1, true, false), ChanType(elem2, true, _))
          if canHold(elem1, elem2)     => true
        //sending channels are contravariant in their element types
        case (ChanType(elem1, false, true), ChanType(elem2, _, true))
          if canHold(elem2, elem1)     => true
        //function types are covariant in results and contravariant in parameters
        case (FuncType(ps1, rs1, vadic1), FuncType(ps2, rs2, vadic2)) if (
          vadic1 == vadic2 &&
          ps1.zipAll(ps2, TopType, BottomType).forall {
            case (param1, param2) =>    param1 =>> param2
          } &&
          rs1.zipAll(rs2, BottomType, TopType).forall {
            case (reslt1, reslt2) =>    reslt1 <<= reslt2
          }
        )                              => true
        case _ => false
      } }
    ) ||
    canHold(t1.underlying, t2.underlying) && !(
      t1.isInstanceOf[TypeName] &&
      t2.isInstanceOf[TypeName]
    )
}

sealed abstract class Semantics
  case object Value     extends Semantics
  case object Reference extends Semantics
  case object Primitive extends Semantics


case object TopType     extends Type
case object NilType     extends Type
case object BottomType  extends Type

trait Nilable extends Type {
  val nilable: Boolean = true
}
