package jgo.compiler
package interm

import symbols._

case class Package(name: String) extends Symbol {
  override def toString = "package " + name
}
