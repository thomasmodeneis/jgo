package jgo.compiler
package interm
package member

import types._

import scala.{collection => coll}
import coll.{mutable => mut}
import coll.{immutable => imm}

class MembersBuilder {
    var map: mut.MapBuilder[String, Member, imm.Map[String, Member]] =
      new mut.MapBuilder(imm.Map())
}
