package jgo.compiler
package interm
package member

import types.Type

import scala.{collection => coll}
import coll.{mutable => mut}
 
trait Membered extends PartialFunction[String, Member] {
  self: Type =>
  
  val members:    Map[String, Member] //yes, immutable map
  
  def get(name: String):      Option[Member] = members get name
  def contains(name: String): Boolean        = members contains name
  
  final def apply(name: String):       Member  = get(name).get
  final def isDefinedAt(name: String): Boolean = contains(name)
}
