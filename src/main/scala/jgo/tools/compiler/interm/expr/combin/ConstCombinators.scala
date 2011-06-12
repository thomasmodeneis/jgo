package jgo.tools.compiler
package interm
package expr
package combin

trait ConstCombinators extends UntypedConstCombinators with ConstTypeCheckOverrides {
  def constant(e: Expr)(pos: Pos) = e match {
    case c: ConstExpr => result(c)
    case _ => problem("expression must be constant")(pos)
  }
}
