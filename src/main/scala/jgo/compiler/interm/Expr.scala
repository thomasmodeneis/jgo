package jgo.compiler
package interm

import types._
import codeseq._

sealed abstract class Expr(val typeOf: Type)

case class ExprCode(code: CodeBuilder, typeOf: Type) extends Expr(typeOf)

case class BoolExpr(tree: BoolTree) extends Expr(Bool)

case class ConstExpr(value: Any, typeOf: ConstType) extends Expr(typeOf)
