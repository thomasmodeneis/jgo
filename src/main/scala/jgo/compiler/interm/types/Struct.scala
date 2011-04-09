package jgo.compiler
package interm
package types

import member._

case class StructType(fields: List[StructType.Field]) extends Type {
  val semantics = Value
  
  import StructType.{RegularField, EmbeddedField}
  
  private type Mbrs = Map[String, Member]
  private val EmptyMbrs: Mbrs = Map.empty
  private def addTo(mbrs: Mbrs, pMbrs: Mbrs, field: StructType.Field): (Mbrs, Mbrs) = field match {
    case RegularField(n, t, tag) =>
      val fPair = (n, Field(n, t, tag))
      (mbrs + fPair, pMbrs + fPair)
    case EmbeddedField(n, tn, isP, tag) =>
      val t  = if (isP) PointerType(tn) else tn
      val f0 = Field(n, t, tag)
      val f0Pair = (n, f0)
      var (mbrs2, pMbrs2) = (mbrs + f0Pair, pMbrs + f0Pair)
      if (!isP) {
        for (m <- f0.typeOf.members)
          mbrs2 += m     //BUG!!! does not recursively add embedded fields of m, if appropriate
        for (m <- f0.typeOf.ptrMembers)
          pMbrs2 += m
      }
      else
        for (m <- f0.typeOf.ptrMembers) {
          mbrs2  += m
          pMbrs2 += m
        }
      (mbrs2, pMbrs2)
  }
  
  val (members, ptrMembers) = (fields foldLeft ((EmptyMbrs, EmptyMbrs))) {
    case ((m, pM), f) => addTo(m, pM, f)
  }
}

object StructType {
  sealed abstract class Field(
    val name:   String,
    val typeOf: Type,
    val tag:    Option[String] = None
  )
  
  case class RegularField(
    name:   String,
    typeOf: Type,
    tag:    Option[String] = None)
  extends Field(name, typeOf, tag)
  
  case class EmbeddedField(
    name:   String,
    typeOf: TypeName,
    isPtr:  Boolean,
    tag:    Option[String] = None)
  extends Field(name, typeOf, tag)
}
