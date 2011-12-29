package jgo.tools.compiler

sealed trait Name {
  def unqualifiedName: String
  def fullName: String
  
  override def toString = fullName
}

case class LocalName(unqualifiedName: String) extends Name {
  def fullName = unqualifiedName
}

case class PkgLevelName(pkgs: List[String], unqualifiedName: String) extends Name {
  val fullName = (pkgs mkString ".") + unqualifiedName
}
