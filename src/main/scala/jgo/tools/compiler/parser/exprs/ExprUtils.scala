package jgo.tools.compiler
package parser
package exprs

import interm.types._
import interm.codeseq._
import interm.expr._
import interm.expr.Combinators._

trait ExprUtils {
  self: Base =>
  
  protected implicit def convTuple[A, R](f: (A, Pos) => R): ((A, Pos)) => R =
    f.tupled
  
  protected implicit def convPrefix[A, R](f: A => Pos => Err[R]): (Pos ~ Err[A]) => Err[R] = {
    case p ~ aErr =>
      for {
        a <- aErr
        res <- f(a)(p)
      } yield res
  }
  
  protected implicit def convSuffix[A, R](f: A => Pos => Err[R]): (Err[A] ~ Pos) => Err[R] = {
    case aErr ~ p =>
      for {
        a <- aErr
        res <- f(a)(p)
      } yield res
  }
  
  protected implicit def convBinary[A, B, R](f: (A, B) => Pos => Err[R]): (Err[A] ~ Pos ~ Err[B]) => Err[R] = {
    case aErr ~ p ~ bErr =>
      for {
        (a, b) <- (aErr, bErr)
        res <- f(a, b)(p)
      } yield res
  }
  
  protected implicit def convTernary[A, B, C, R](f: (A, B, C) => Pos => Err[R]): (Err[A] ~ Pos ~ Err[B] ~ Err[C]) => Err[R] = {
    case aErr ~ p ~ bErr ~ cErr =>
      for {
        (a, b, c) <- (aErr, bErr, cErr)
        res <- f(a, b, c)(p)
      } yield res
  }
  
  protected implicit def convSlice(f: (Expr, Option[Expr], Option[Expr]) => Pos => Err[Expr])
      : (Err[Expr] ~ Pos ~ Option[Err[Expr]] ~ Option[Err[Expr]]) => Err[Expr] = {
    case e1Err ~ p ~ e2Ugly ~ e3Ugly =>
      val e2Err = Err.liftOpt(e2Ugly)
      val e3Err = Err.liftOpt(e3Ugly)
      for {
        (e1, e2, e3) <- (e1Err, e2Err, e3Err)
        res <- f(e1, e2, e3)(p)
      } yield res
  }
  
  protected def map[A, B](f: A => B): Err[A] => Err[B] = _ map f
}
