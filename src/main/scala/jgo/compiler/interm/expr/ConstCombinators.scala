package jgo.compiler
package interm
package expr

trait ConstCombinators extends UntypedConstCombinators with ConstTypeCheckOverrides {
  def constant(e: Expr)(pos: Pos) = e match {
    case c: ConstExpr => result(c)
    case _ => problem("expression must be constant")(pos)
  }
}
