package jgo.compiler
package parser

import interm._
import types._

trait TypeUtils {
  self: Base =>
  
  def badType(msg: String, args: Any*): TypeError.type = {
    recordErr(msg, args: _*)
    TypeError
  }
}
