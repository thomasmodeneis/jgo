package jgo.compiler
package parser
package exprs

import interm.types._
import interm.codeseq._
import interm.expr._
import interm.expr.Combinators._

trait ExprUtils {
  self: Base =>
  
  protected implicit def convPrefix[A, R](f: A => Pos => M[R]): (Pos ~ M[A]) => M[R] = {
    case p ~ aM => for {
      a <- aM
      res <- f(a)(p)
    }
    yield res
  }
  
  protected implicit def convSuffix[A, R](f: A => Pos => M[R]): (M[A] ~ Pos) => M[R] = {
    case aM ~ p => for {
      a <- aM
      res <- f(a)(p)
    }
    yield res
  }
  
  protected implicit def convBinary[A, B, R](f: (A, B) => Pos => M[R]): (M[A] ~ Pos ~ M[B]) => M[R] = {
    case aM ~ p ~ bM => for {
      (a, b) <- (aM, bM)
      res <- f(a, b)(p)
    }
    yield res
  }
  
  protected implicit def convSlice(f: (Expr, Option[Expr], Option[Expr]) => Pos => M[Expr])
      : (M[Expr] ~ Pos ~ Option[M[Expr]] ~ Option[M[Expr]]) => M[Expr] = {
    case e1M ~ p ~ e2Ugly ~ e3Ugly =>
      //val (e2M: M[Option[Expr]], e3M: M[Option[Expr]]) = (e2Ugly, e3Ugly) //conversion!
      val e2M: M[Option[Expr]] = e2Ugly
      val e3M: M[Option[Expr]] = e3Ugly
      for {
        (e1, e2, e3) <- (e1M, e2M, e3M)
        res <- f(e1, e2, e3)(p)
      }
      yield res
  }
  
  protected def map[A, B](f: A => B): M[A] => M[B] = _ map f
}
