package jgo.compiler
package interm

import java.lang.Integer.toHexString

class Label(val tag: String) {
  override def toString = tag + " " + toHexString(hashCode & 0xffff)
}
