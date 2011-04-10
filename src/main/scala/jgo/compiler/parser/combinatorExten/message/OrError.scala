package jgo.compiler
package parser
package combinatorExten
package message

import scala.util.parsing.input.{Position, Positional}

object OrError {
  implicit def any2orError[T](cont: T): OrError[T] = NoError(cont)
}

sealed abstract class OrError[+T] extends Positional {
  val lifted: Option[T]
}

case class ErrorPresent(err: ErrorMsg) extends OrError[Nothing] {
  val lifted = None
  override def setPos(pos: Position): this.type = {
    err.setPos(pos)
    super.setPos(pos)
  }
}

case class NoError[+T](cont: T) extends OrError[T] {
  val lifted = Some(cont)
  override def setPos(pos: Position): this.type = {
    cont match {
      case c: Positional => c.setPos(pos)
    }
    super.setPos(pos)
  }
}
