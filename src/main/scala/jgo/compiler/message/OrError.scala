package jgo.compiler
package message

import scala.util.parsing.input.{Position, Positional}


sealed abstract class OrError[+T] extends Positional {
  val lifted: Option[T]
  
  def map    [U](f: T => U):          OrError[U]
  def flatMap[U](f: T => OrError[U]): OrError[U]
  def foreach[U](f: T => U):          Unit
}

case class NoError[+T](cont: T) extends OrError[T] {
  val lifted = Some(cont)
  
  override def setPos(pos: Position): this.type = {
    cont match {
      case c: Positional => c.setPos(pos)
    }
    super.setPos(pos)
  }
  
  def map    [U](f: T => U)          = NoError(f(cont))
  def flatMap[U](f: T => OrError[U]) = f(cont)
  def foreach[U](f: T => U) { f(cont) }
}

case class ErrorPresent(err: ErrorMsg) extends OrError[Nothing] {
  val lifted = None
  
  override def setPos(pos: Position): this.type = {
    err.setPos(pos)
    super.setPos(pos)
  }
  
  def map    [U](f: Nothing => U)          = this
  def flatMap[U](f: Nothing => OrError[U]) = this
  def foreach[U](f: Nothing => U) { }
}
