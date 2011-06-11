package jgo.tools.compiler
package interm

import types._
import symbol._
import instr._
import instr.TypeConversions._
import codeseq._

import scala.util.parsing.input.Position

package object expr {
  //implicit def mappedToExpr[A](ls: List[A])(implicit ev: A => Expr): List[Expr] =
    //ls map ev
}
