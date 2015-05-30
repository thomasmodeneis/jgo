package jgo.tools.compiler
package interm
package symbol

case class PackageSymbol(name: String) extends Symbol {
  override def toString = "package " + name
}
