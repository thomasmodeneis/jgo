package jgo.compiler
package interm
package expr

trait ConstCombinators extends UntypedConstCombinators with ConstTypeCheckOverrides {
  def constant(e: Expr)(pos: Pos) = e match {
    case c: ConstExpr => Result(c)
    case _ => Problem("expression must be constant")(pos)
  }
}
