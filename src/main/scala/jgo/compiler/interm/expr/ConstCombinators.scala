package jgo.compiler
package interm
package expr

import message._
import message.Messaged._

import types._
import instr._
import instr.TypeConversions._
import codeseq._

private trait ConstCombinators extends Combinators with ConstTypeCheckOverrides {
  /*
  private def untypedToTyped(c: UntypedRealConst, t: RealType) (implicit pos: Pos) =
    if (c.canFitIn(t)) Result(TypedRealConst(c.value, t))
    else Problem("value %s not not in range for type %s", c.value, t)
  
  private def sameNumericConst(e1: Expr, e2: Expr)
                              (f: (BigDecimal, BigDecimal) => BigDecimal)
                              (implicit pos: Pos): Option[M[Constant]] = //this is really hackish...
    (e1, e2) match {
      case (TypedRealConst(v1, t1), TypedRealConst(v2, t2)) =>
        if (t1 == t2) Some(Result(TypedRealConst(f(v1, v2), t1)))
        else Some(Problem("left and right operands have differing types %s and %s", t1, t2))
      
      case (UntypedRealConst(v1), UntypedRealConst(v2)) =>
        Some(Result(UntypedRealConst(f(v1, v2))))
      
      case (TypedRealConst(v1, t), uc @ UntypedRealConst(v2)) =>
        if (uc.canFitIn(t)) Some(Result(TypedConst(f(v1, v2), t)))
        else Some(Problem("value %s not not in range for type %s", uc.value, t))
        
      case (uc @ UntypedRealConst(v1), TypedRealConst(v2, t)) =>
        if (uc.canFitIn(t)) Some(Result(TypedConst(f(v1, v2), t)))
        else Some(Problem("value %s not not in range for type %s", uc.value, t))
      
      case _ => None
    }
  
  private def sameIntegralConst(e1: Expr, e2: Expr)
                               (f: (BigDecimal, BigDecimal) => BigDecimal)
                               (implicit pos: Pos): Option[M[Constant]] =
    (e1, e2) match {
      case (TypedRealConst(v1, t1: IntegralType), TypedRealConst(v2, t2: IntegralType)) =>
        if (t1 == t2) Some(Result(TypedRealConst(f(v1, v2), t1)))
        else Some(Problem("left and right operands have differing types %s and %s", t1, t2))
      
      case (UntypedRealConst(v1), UntypedRealConst(v2)) =>
        Some(Result(UntypedRealConst(f(v1, v2))))
      
      case (TypedRealConst(v1, t: IntegralType), uc @ UntypedRealConst(v2)) =>
        if (uc.canFitIn(t)) Some(Result(TypedConst(f(v1, v2), t)))
        else Some(Problem("value %s not not in range for type %s", uc.value, t))
        
      case (uc @ UntypedRealConst(v1), TypedRealConst(v2, t: IntegralType)) =>
        if (uc.canFitIn(t)) Some(Result(TypedConst(f(v1, v2), t)))
        else Some(Problem("value %s not not in range for type %s", uc.value, t))
      
      case _ => None
    }
  */
  
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
    case (StringConst(v1), StringConst(v2)) => Result(BoolConst(v1 == v2))
    case (BoolConst(v1),   BoolConst(v2))   => Result(BoolConst(v1 == v2))
    case (IntConst(v1),    IntConst(v2))    => Result(BoolConst(v1 == v2))
    case (FloatConst(v1),  FloatConst(v2))  => Result(BoolConst(v1 == v2))
    case _ => super.compEq(e1, e2)
  }
  abstract override def compNe(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (StringConst(v1), StringConst(v2)) => Result(BoolConst(v1 != v2))
    case (BoolConst(v1),   BoolConst(v2))   => Result(BoolConst(v1 != v2))
    case (IntConst(v1),    IntConst(v2))    => Result(BoolConst(v1 != v2))
    case (FloatConst(v1),  FloatConst(v2))  => Result(BoolConst(v1 != v2))
    case _ => super.compNe(e1, e2)
  }
  abstract override def compLt(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(v1),   IntConst(v2))   => Result(BoolConst(v1 < v2))
    case (FloatConst(v1), FloatConst(v2)) => Result(BoolConst(v1 < v2))
    case _ => super.compLt(e1, e2)
  }
  abstract override def compLeq(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(v1),   IntConst(v2))   => Result(BoolConst(v1 <= v2))
    case (FloatConst(v1), FloatConst(v2)) => Result(BoolConst(v1 <= v2))
    case _ => super.compLeq(e1, e2)
  }
  abstract override def compGt(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(v1),   IntConst(v2))   => Result(BoolConst(v1 > v2))
    case (FloatConst(v1), FloatConst(v2)) => Result(BoolConst(v1 > v2))
    case _ => super.compGt(e1, e2)
  }
  abstract override def compGeq(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(v1),   IntConst(v2))   => Result(BoolConst(v1 >= v2))
    case (FloatConst(v1), FloatConst(v2)) => Result(BoolConst(v1 >= v2))
    case _ => super.compGeq(e1, e2)
  }
  
  
  abstract override def plus(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (StringConst(str1), StringConst(str2)) => Result(StringConst(str1 + str2))
    case (IntConst(i1), IntConst(i2)) => Result(IntConst(i1 + i2))
    case (FloatConst(f1), FloatConst(f2)) => Result(FloatConst(f1 + f2))
    case _ => super.plus(e1, e2)
    //case _ => sameNumericConst(e1, e2) { _ + _ } getOrElse super.plus(e1, e2)
  }
  
  abstract override def minus(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(i1),   IntConst(i2))   => Result(IntConst(i1 - i2))
    case (FloatConst(f1), FloatConst(f2)) => Result(FloatConst(f1 - f2))
    case _ => super.minus(e1, e2)
  }
    //sameNumericConst(e1, e2) { _ - _ } getOrElse super.minus(e1, e2)
  
  abstract override def times(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(i1),   IntConst(i2))   => Result(IntConst(i1 * i2))
    case (FloatConst(f1), FloatConst(f2)) => Result(FloatConst(f1 * f2))
    case _ => super.times(e1, e2)
  }
    //sameNumericConst(e1, e2) { _ * _ } getOrElse super.times(e1, e2)
  
  abstract override def div(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(i1),   IntConst(i2))   => Result(IntConst(i1 / i2))
    case (FloatConst(f1), FloatConst(f2)) => Result(FloatConst(f1 / f2))
    case _ => super.div(e1, e2)
  }
    //sameNumericConst(e1, e2) { _ / _ } getOrElse super.div(e1, e2)
  
  abstract override def mod(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(i1),   IntConst(i2))   => Result(IntConst(i1 % i2))
    case _ => super.mod(e1, e2)
  }
    //sameIntegralConst(e1, e2) { _ % _ } getOrElse super.mod(e1, e2)
  
  abstract override def pos(e: Expr) (implicit pos: Pos) = e match {
    case i: IntConst   => i
    case f: FloatConst => f
    case _ => super.pos(e)
  }
  
  abstract override def neg(e: Expr) (implicit pos: Pos) = e match {
    case IntConst(i)   => IntConst(-i)
    case FloatConst(f) => FloatConst(-f)
    case _ => super.neg(e)
  }
  
  
  abstract override def bitAnd(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(i1), IntConst(i2)) => Result(IntConst(i1 & i2))
    case _ => super.bitAnd(e1, e2)
  }
  abstract override def bitAndNot(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(i1), IntConst(i2)) => Result(IntConst(i1 & ~i2))
    case _ => super.bitAndNot(e1, e2)
  }
  abstract override def bitOr(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(i1), IntConst(i2)) => Result(IntConst(i1 | i2))
    case _ => super.bitOr(e1, e2)
  }
  abstract override def bitXor(e1: Expr, e2: Expr) (implicit pos: Pos) = (e1, e2) match {
    case (IntConst(i1), IntConst(i2)) => Result(IntConst(i1 ^ i2))
    case _ => super.bitXor(e1, e2)
  }
  //def shiftL(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  //def shiftR(e1: Expr, e2: Expr) (implicit pos: Pos): M[Expr]
  abstract override def bitCompl(e: Expr) (implicit pos: Pos) = e match {
    case IntConst(i) => IntConst(~i)
    case _ => super.bitCompl(e)
  }
}
