package jgo.compiler
package parser
package exprs

import interm.types._
import interm.codeseq._
import interm.expr._
import interm.expr.Combinators._

trait ExprUtils {
  self: Base =>
  
  protected implicit def convUnary(f: Expr => Pos => M[Expr]): (Pos ~ M[Expr]) => M[Expr] = {
    case p ~ eM => for {
      e <- eM
      res <- f(e)(p)
    } yield res
  }
  
  protected implicit def convBinary(f: (Expr, Expr) => Pos => M[Expr]): (M[Expr] ~ Pos ~ M[Expr]) => M[Expr] = {
    case e1M ~ p ~ e2M => for {
      (e1, e2) <- together2(e1M, e2M)
      res <- f(e1, e2)(p)
    } yield res
  }
  
  protected implicit def convSlice(f: (Expr, Option[Expr], Option[Expr]) => Pos => M[Expr])
      : (M[Expr] ~ Pos ~ Option[M[Expr]] ~ Option[M[Expr]]) => M[Expr] = {
    case e1M ~ p ~ e2OptM ~ e3OptM => for {
      (e1, e2, e3) <- together3(e1M, e2OptM, e3OptM) //conversion!
      res <- f(e1, e2, e3)(p)
    } yield res
  }
}
