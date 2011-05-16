package jgo.compiler
package interm

object LabelGroup {
  private var curId: Long = 1
  
  val User = new LabelGroup(0)
}

final class LabelGroup private(id: Long) {
  def this() = {
    this(LabelGroup.curId)
    LabelGroup.curId += 1
  }
  
  def designation: String = id.toString
}

sealed class Label(val tag: String, val group: LabelGroup) {
  def this(tag: String) = this(tag, new LabelGroup)
  override def toString = tag + " " + group.designation
}

final class UserLabel(val name: String) extends Label(name, LabelGroup.User) {
  override def toString = name
}
