package jgo.compiler
package parser

import lexer.Token

import scala.util.parsing.input.Reader

abstract class InputProcessor[T] {
  val in: Reader[Token]
  
  val syntactical: Base
  def process(): T
  
  //final def apply(): ProcResult[T]
}
