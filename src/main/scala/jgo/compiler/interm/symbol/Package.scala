package jgo.compiler
package interm
package symbol

case class Package(name: String) extends Symbol {
  override def toString = "package " + name
}
