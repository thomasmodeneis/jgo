package jgo.tools.compiler
package interm
package expr
package combin

//TODO:  Add support for constant expressions consisting of typed constants!
//Mid-high priority.
trait ConstCombinators extends UntypedConstCombinators with ConstTypeCheckOverrides {
  def constant(e: Expr)(pos: Pos) = e match {
    case c: ConstExpr => result(c)
    case _ => problem("expression must be constant")(pos)
  }
}
