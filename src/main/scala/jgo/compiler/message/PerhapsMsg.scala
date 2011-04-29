package jgo.compiler
package message

import scala.util.parsing.input.{Position, Positional}

sealed abstract class PerhapsMsg[+T] extends Positional {
  val cont: T
}

case class WithMsg[+T](msg: Message, cont: T) extends PerhapsMsg[T] {
  override def setPos(pos: Position): this.type = {
    msg.setPos(pos)
    cont match {
      case c: Positional => c.setPos(pos)
    }
    super.setPos(pos)
  }
}

case class NoMsg[+T](cont: T) extends PerhapsMsg[T] {
  override def setPos(pos: Position): this.type = {
    cont match {
      case c: Positional => c.setPos(pos)
    }
    super.setPos(pos)
  }
}
  