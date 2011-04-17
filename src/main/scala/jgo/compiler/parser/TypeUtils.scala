package jgo.compiler
package parser

import interm._
import types._

trait TypeUtils {
  self: Base =>
  
  def badType(msg: String, args: AnyRef*): TypeError.type = {
    recordErr(msg, args: _*)
    TypeError
  }
}
