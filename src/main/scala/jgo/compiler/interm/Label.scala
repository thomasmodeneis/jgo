package jgo.compiler
package interm

import java.lang.Integer.toHexString

object LabelGroup {
  private var curId: Long = 1
  
  object User extends LabelGroup(0)
}

class LabelGroup private(val id: Long){
  def this() = { this(LabelGroup.curId); LabelGroup.curId += 1 }
}

class Label(val tag: String, val group: LabelGroup) {
  def this(tag: String) = this(tag, new LabelGroup)
  override def toString = tag + " " + group.id
}
