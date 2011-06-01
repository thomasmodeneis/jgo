package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

trait ConstCombinators extends Combinators with ConstTypeCheckOverrides {
  //defined in jgo.compiler.package as M
  //private implicit def wrapInResult[T](v: T): M[T] = Result(v)
  
  
  def const(e: Expr) (implicit pos: Pos) = e match {
    case c: ConstExpr => Result(c)
    case _ => Problem("expression must be constant")
  }
  
  
  abstract override def and(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (BoolConst(b1), BoolConst(b2)) => BoolConst(b1 && b2)
    case _ => super.and(e1, e2)
  }
  
  abstract override def or(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (BoolConst(b1), BoolConst(b2)) => BoolConst(b1 || b2)
    case _ => super.or(e1, e2)
  }
  
  abstract override def not(e: Expr) (implicit pos: Pos) = e match {
    case BoolConst(b) => BoolConst(!b)
    case _ => super.not(e)
  }
  
  
  abstract override def compEq(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (StringConst(v1), StringConst(v2)) => BoolConst(v1 == v2)
    case (BoolConst(v1),   BoolConst(v2))   => BoolConst(v1 == v2)
    case (IntConst(v1),    IntConst(v2))    => BoolConst(v1 == v2)
    case (FloatConst(v1),  FloatConst(v2))  => BoolConst(v1 == v2)
    
    case (UntypedIntegralConst(i1),    UntypedIntegralConst(i2))    => BoolConst(i1 == i2)
    case (UntypedRealConst(r1),        UntypedRealConst(r2))        => BoolConst(r1 == r2)
    case (UntypedNumericConst(r1, i1), UntypedNumericConst(r2, i2)) => BoolConst(r1 == r2 && i1 == i2)
    
    case _ => super.compEq(e1, e2)
  }
  
  abstract override def compNe(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (StringConst(v1), StringConst(v2)) => BoolConst(v1 != v2)
    case (BoolConst(v1),   BoolConst(v2))   => BoolConst(v1 != v2)
    case (IntConst(v1),    IntConst(v2))    => BoolConst(v1 != v2)
    case (FloatConst(v1),  FloatConst(v2))  => BoolConst(v1 != v2)
    
    case (UntypedIntegralConst(i1),    UntypedIntegralConst(i2))    => BoolConst(i1 != i2)
    case (UntypedRealConst(r1),        UntypedRealConst(r2))        => BoolConst(r1 != r2)
    case (UntypedNumericConst(r1, i1), UntypedNumericConst(r2, i2)) => BoolConst(r1 != r2 || i1 != i2)
    
    case _ => super.compNe(e1, e2)
  }
  
  abstract override def compLt(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(v1),   IntConst(v2))   => BoolConst(v1 < v2)
    case (FloatConst(v1), FloatConst(v2)) => BoolConst(v1 < v2)
    
    case (UntypedIntegralConst(i1), UntypedIntegralConst(i2)) => BoolConst(i1 < i2)
    case (UntypedRealConst(r1),     UntypedRealConst(r2))     => BoolConst(r1 < r2)
    
    case _ => super.compLt(e1, e2)
  }
  
  abstract override def compLeq(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(v1),   IntConst(v2))   => BoolConst(v1 <= v2)
    case (FloatConst(v1), FloatConst(v2)) => BoolConst(v1 <= v2)
    
    case (UntypedIntegralConst(i1), UntypedIntegralConst(i2)) => BoolConst(i1 <= i2)
    case (UntypedRealConst(r1),     UntypedRealConst(r2))     => BoolConst(r1 <= r2)
    
    case _ => super.compLeq(e1, e2)
  }
  
  abstract override def compGt(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(v1),   IntConst(v2))   => BoolConst(v1 > v2)
    case (FloatConst(v1), FloatConst(v2)) => BoolConst(v1 > v2)
    
    case (UntypedIntegralConst(i1), UntypedIntegralConst(i2)) => BoolConst(i1 > i2)
    case (UntypedRealConst(r1),     UntypedRealConst(r2))     => BoolConst(r1 > r2)
    
    case _ => super.compGt(e1, e2)
  }
  
  abstract override def compGeq(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(v1),   IntConst(v2))   => BoolConst(v1 >= v2)
    case (FloatConst(v1), FloatConst(v2)) => BoolConst(v1 >= v2)
    
    case (UntypedIntegralConst(i1), UntypedIntegralConst(i2)) => BoolConst(i1 >= i2)
    case (UntypedRealConst(r1),     UntypedRealConst(r2))     => BoolConst(r1 >= r2)
    
    case _ => super.compGeq(e1, e2)
  }
  
  
  abstract override def plus(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (StringConst(str1), StringConst(str2)) => StringConst(str1 + str2)
    
    case (IntConst(i1), IntConst(i2))     => IntConst(i1 + i2)
    case (FloatConst(f1), FloatConst(f2)) => FloatConst(f1 + f2)
    
    case (UntypedIntegralConst(i1),    UntypedIntegralConst(i2))    => UntypedIntegralConst(i1 + i2)
    case (UntypedRealConst(r1),        UntypedRealConst(r2))        => UntypedFloatingConst(r1 + r2)
    case (UntypedNumericConst(r1, i1), UntypedNumericConst(r2, i2)) => UntypedComplexConst(r1 + r2, i1 + i2)
    
    case _ => super.plus(e1, e2)
  }
  
  abstract override def minus(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(i1),   IntConst(i2))   => IntConst(i1 - i2)
    case (FloatConst(f1), FloatConst(f2)) => FloatConst(f1 - f2)
    
    case (UntypedIntegralConst(i1),    UntypedIntegralConst(i2))    => UntypedIntegralConst(i1 - i2)
    case (UntypedRealConst(r1),        UntypedRealConst(r2))        => UntypedFloatingConst(r1 - r2)
    case (UntypedNumericConst(r1, i1), UntypedNumericConst(r2, i2)) => UntypedComplexConst(r1 - r2, i1 - i2)
    
    case _ => super.minus(e1, e2)
  }
  
  abstract override def times(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(i1),   IntConst(i2))   => Result(IntConst(i1 * i2))
    case (FloatConst(f1), FloatConst(f2)) => Result(FloatConst(f1 * f2))
    
    case (UntypedIntegralConst(i1),    UntypedIntegralConst(i2))    => UntypedIntegralConst(i1 * i2)
    case (UntypedRealConst(r1),        UntypedRealConst(r2))        => UntypedFloatingConst(r1 * r2)
    case (UntypedNumericConst(r1, i1), UntypedNumericConst(r2, i2)) => UntypedComplexConst(r1*r2 - i1*i2,
                                                                                           r1*i2 + r2*i1)
    
    case _ => super.times(e1, e2)
  }
  
  abstract override def div(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(i1),   IntConst(i2))   => IntConst(i1 / i2)
    case (FloatConst(f1), FloatConst(f2)) => FloatConst(f1 / f2)
    
    case (UntypedIntegralConst(i1),    UntypedIntegralConst(i2))    => UntypedIntegralConst(i1 / i2)
    case (UntypedRealConst(r1),        UntypedRealConst(r2))        => UntypedFloatingConst(r1 / r2)
    case (UntypedNumericConst(r1, i1), UntypedNumericConst(r2, i2)) =>
      val normSq2 = r2*r2 + i2*i2
      val r = (r1*r2 + i1*i2) / normSq2
      var i = (i2*r1 - r2*i1) / normSq2
      UntypedComplexConst(r, i)
    
    case _ => super.div(e1, e2)
  }
  
  abstract override def mod(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(i1), IntConst(i2)) => IntConst(i1 % i2)
    case (UntypedIntegralConst(i1), UntypedIntegralConst(i2)) => UntypedIntegralConst(i1 % i2)
    case _ => super.mod(e1, e2)
  }
  
  abstract override def positive(e: Expr) (implicit pos: Pos) = e match {
    case i: IntConst   => i
    case f: FloatConst => f
    
    case i: UntypedIntegralConst => i
    case f: UntypedFloatingConst => f
    case c: UntypedComplexConst  => c
    
    case _ => super.positive(e)
  }
  
  abstract override def negative(e: Expr) (implicit pos: Pos) = e match {
    case IntConst(i)   => IntConst(-i)
    case FloatConst(f) => FloatConst(-f)
    
    case UntypedIntegralConst(i)   => UntypedIntegralConst(-i)
    case UntypedFloatingConst(r)   => UntypedFloatingConst(-r)
    case UntypedComplexConst(r, i) => UntypedComplexConst(-r, -i)
    
    case _ => super.negative(e)
  }
  
  
  abstract override def bitAnd(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(i1), IntConst(i2)) => IntConst(i1 & i2)
    case (UntypedIntegralConst(i1), UntypedIntegralConst(i2)) => UntypedIntegralConst(i1 & i2)
    case _ => super.bitAnd(e1, e2)
  }
  
  abstract override def bitAndNot(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(i1), IntConst(i2)) => IntConst(i1 & ~i2)
    case (UntypedIntegralConst(i1), UntypedIntegralConst(i2)) => UntypedIntegralConst(i1 &~ i2)
    case _ => super.bitAndNot(e1, e2)
  }
  
  abstract override def bitOr(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(i1), IntConst(i2)) => IntConst(i1 | i2)
    case (UntypedIntegralConst(i1), UntypedIntegralConst(i2)) => UntypedIntegralConst(i1 | i2)
    case _ => super.bitOr(e1, e2)
  }
  
  abstract override def bitXor(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(i1), IntConst(i2)) => IntConst(i1 ^ i2)
    case (UntypedIntegralConst(i1), UntypedIntegralConst(i2)) => UntypedIntegralConst(i1 ^ i2)
    case _ => super.bitXor(e1, e2)
  }
  
  //def shiftL(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  //def shiftR(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  
  abstract override def bitCompl(e: Expr) (implicit pos: Pos) = e match {
    case IntConst(i) => IntConst(~i)
    case UntypedIntegralConst(i) => UntypedIntegralConst(i~) //BigInt defines ~ instead of unary_~. Filed a bug report.
    case _ => super.bitCompl(e)                              //https://issues.scala-lang.org/browse/SI-4659
  }
}
