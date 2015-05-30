package jgo.tools.compiler
package interm
package types


case class StructType(fields: List[FieldDesc]) extends UnderType {
  val semantics = Value
  
  private val (rootMembers, embeddeds) = {
    var mbrs: Map[String, Member] = Map()
    var embs: Set[FieldMember] = Set()
    
    for (field <- fields) field match {
      case RegularFieldDesc(n, t, tag) =>
        mbrs += (n -> FieldMember(this, n, t))
      
      case EmbeddedFieldDesc(n, tn, isP, tag) =>
        val t = if (isP) PointerType(tn) else tn
        val member = FieldMember(this, n, t)
        mbrs += (n -> member)
        embs += member
    }
    (mbrs, embs)
  }
  
  override def selectMember(name: String) =
    rootMembers.get(name) orElse {
      val mapping =
        (f: FieldMember) => f.typeOf.selectMember(name) map (EmbeddedMember(f, _))
      //FIXME: Infinite loop on self-embedded struct when selecting nonexistent member.
      embeddeds.map(mapping).fold(None: Option[Member]) { Function.untupled {
        case (Some(a), None) => Some(a)
        case (None, Some(b)) => Some(b)
        case (Some(a), Some(b)) if a.depth < b.depth => Some(a)
        case (Some(a), Some(b)) if b.depth < a.depth => Some(b)
        case _ => None
      }}
    }
}


sealed abstract class FieldDesc {
  val name:   String
  val typeOf: Type
  val tag:    Option[String]

  //TODO: Does this need refinement for unicode?
  def isPublic: Boolean = !name(0).isLower
}

case class RegularFieldDesc(
  name:   String,
  typeOf: Type,
  tag:    Option[String] = None)
extends FieldDesc

case class EmbeddedFieldDesc(
  name:   String,
  typeOf: NamedType,
  isPtr:  Boolean,
  tag:    Option[String] = None)
extends FieldDesc
