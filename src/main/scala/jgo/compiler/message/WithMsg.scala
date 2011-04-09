package jgo.compiler
package message

import scala.util.parsing.input.{Position, Positional}

case class WithMsg[+T](msg: Message, cont: T) extends Positional {
  override def setPos(pos: Position): this.type = {
    msg.setPos(pos)
  //cont.setPos(pos)
    super.setPos(pos)
  }
}
