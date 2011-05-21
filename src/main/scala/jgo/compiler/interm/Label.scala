package jgo.compiler
package interm

object LabelGroup {
  private var curId: Long = 1
  
  val User = new LabelGroup("<user>") //we should never actually see that string
}

final class LabelGroup(val id: String) {
  def this() = {
    this(LabelGroup.curId.toString)
    LabelGroup.curId += 1
  }
}

sealed class Label(val tag: String, val group: LabelGroup) {
  def this(tag: String) = this(tag, new LabelGroup)
  override def toString = tag + " " + group.id
}

final class UserLabel(val name: String) extends Label(name, LabelGroup.User) {
  override def toString = name
}
