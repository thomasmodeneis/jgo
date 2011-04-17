package jgo.compiler
package parser

import interm._
import types._
import codeseq._
import instr._

trait Expressions extends PrimaryExprs with ExprUtils {
  lazy val expression: PP[Expr] =                       "expression" $
    addExpr //orExpr
  
  //implementation delayed until implementation of BoolTree
  /*lazy val orExpr: PP[Expr]=                 "or-expression: prec 1" $
    ( orExpr ~ ("||" ~> andExpr)  //^^# conv{ BinExpr(op_||, _, _) }
    | andExpr
    )
  
  lazy val andExpr: PP[Expr] =              "and-expression: prec 2" $
    ( andExpr ~ ("&&" ~> relExpr)  //^^# conv{ BinExpr(op_&&, _, _) }
    | relExpr
    )
  
  lazy val relExpr: PP[Expr] =       "relational expression: prec 3" $
    ( relExpr ~ ("==" ~> addExpr)  //^^# conv{ BinExpr(op_==, _, _) }
    | relExpr ~ ("!=" ~> addExpr)  //^^# conv{ BinExpr(op_!=, _, _) }
    | relExpr ~ ("<"  ~> addExpr)  //^^# conv{ BinExpr(op_<, _, _)  }
    | relExpr ~ ("<=" ~> addExpr)  //^^# conv{ BinExpr(op_<=, _, _) }
    | relExpr ~ (">"  ~> addExpr)  //^^# conv{ BinExpr(op_>, _, _)  }
    | relExpr ~ (">=" ~> addExpr)  //^^# conv{ BinExpr(op_>=, _, _) }
    | addExpr
    )*/
  
  lazy val addExpr: PP[Expr] =         "additive expression: prec 4" $
    ( addExpr ~ ("+" ~> multExpr)  ^^ plus
    | addExpr ~ ("-" ~> multExpr)  ^^ minus
    | addExpr ~ ("|" ~> multExpr)  ^^ bitOr
    | addExpr ~ ("^" ~> multExpr)  ^^ bitXor
    | multExpr
    )
  
  lazy val multExpr: PP[Expr] =  "multiplicative expression: prec 5" $
    ( multExpr ~ ("*"  ~> unaryExpr)  ^^ times
    | multExpr ~ ("/"  ~> unaryExpr)  ^^ div
    | multExpr ~ ("%"  ~> unaryExpr)  ^^ mod
    | multExpr ~ ("<<" ~> unaryExpr)  ^^ shiftl
    | multExpr ~ (">>" ~> unaryExpr)  ^^ shiftr
    | multExpr ~ ("&"  ~> unaryExpr)  ^^ bitAnd
    | multExpr ~ ("&^" ~> unaryExpr)  ^^ bitAndNot
    | unaryExpr
    )
  
  lazy val unaryExpr: PP[Expr] =          "unary expression: prec 6" $
    (("+"  ~> unaryExpr) ^^ { ifNumeric(_) { (code, underlNumericT, actualT) => SimpleExpr(code, actualT) } }
    | "-"  ~> unaryExpr  ^^ { ifNumeric(_) { (c, n, t) => SimpleExpr(c |+| Neg(n), t) } }
    | "^"  ~> unaryExpr  ^^ { ifIntegral(_) { (c, i, t) => SimpleExpr(c |+| BitwiseNot(i), t) } }
    | "!"  ~> unaryExpr  //^^# (Not(_))
    | "<-" ~> unaryExpr  ^^ chanRecv
    | "&"  ~> unaryExpr  //^^# (AddrOf(_))
    | "*"  ~> unaryExpr  ^^ deref
    | primaryExpr
    )
    
  lazy val exprList: P[List[Expr]] =               "expression list" $
    rep1sep(expression, ",")
  
  
  private def encat[T <: Type](f: (CodeBuilder, T, Type) => Expr): (CodeBuilder, CodeBuilder, T, Type) => Expr =
    (b1, b2, t0, t) => f(b1 |+| b2, t0, t)
  
  private def encat[T1 <: Type, T2 <: Type](f: (CodeBuilder, T1, T2, Type) => Expr)
    : (CodeBuilder, CodeBuilder, T1, T2, Type) => Expr =
    (b1, b2, t1, t2, t) => f(b1 |+| b2, t1, t2, t)
  
  private def simple(cat: CodeBuilder) =
    (b: CodeBuilder, exprT: Type) => SimpleExpr(b |+| cat, exprT)
  
  private def simple[T <: Type](catF: T => CodeBuilder) =
    (b: CodeBuilder, underlT: T, exprT: Type) => SimpleExpr(b |+| catF(underlT), exprT)
  
  private def simple[T1 <: Type, T2 <: Type](catF: (T1, T2) => CodeBuilder) =
    (b: CodeBuilder, underlT1: T1, underlT2: T2, exprT: Type) => SimpleExpr(b |+| catF(underlT1, underlT2), exprT)
  
  
  private def plus(e1: Expr, e2: Expr): Expr =
    if (e1.t != e2.t)
      badExpr("operands have differing types %s and %s", e1.t, e2.t)
    else e1 match {
      case _ OfType (StringType)  => SimpleExpr(e1.eval |+| e2.eval |+| StrAdd, e1.t)
      case _ OfType (t: NumericType) => SimpleExpr(e1.eval |+| e2.eval |+| Add(t), e1.t)
      case _ => badExpr("operand type %s not numeric or string type", e1.t)
    }
  //Get ready for procedural abstraction, functional programming style!
  private def minus(e1: Expr, e2: Expr): Expr     = ifSameNumeric(e1, e2)(encat(simple(Sub(_))))
  private def times(e1: Expr, e2: Expr): Expr     = ifSameNumeric(e1, e2)(encat(simple(Mul(_))))
  private def div(e1: Expr, e2: Expr): Expr       = ifSameNumeric(e1, e2)(encat(simple(Div(_))))
  private def mod(e1: Expr, e2: Expr): Expr       = ifSameIntegral(e1, e2)(encat(simple(Mod(_))))
  private def bitAnd(e1: Expr, e2: Expr): Expr    = ifSameIntegral(e1, e2)(encat(simple(BitwiseAnd(_))))
  private def bitAndNot(e1: Expr, e2: Expr): Expr = ifSameIntegral(e1, e2)(encat(simple(BitwiseAndNot(_))))
  private def bitOr(e1: Expr, e2: Expr): Expr     = ifSameIntegral(e1, e2)(encat(simple(BitwiseOr(_))))
  private def bitXor(e1: Expr, e2: Expr): Expr    = ifSameIntegral(e1, e2)(encat(simple(BitwiseXor(_))))
  private def shiftl(e1: Expr, e2: Expr): Expr    = ifValidShift(e1, e2)(encat(simple(ShiftL(_, _))))
  private def shiftr(e1: Expr, e2: Expr): Expr    = ifValidShift(e1, e2)(encat(simple(ShiftR(_, _))))
  
  private def pos(expr: Expr): Expr      = ifNumeric(expr)((_, _, _) => SimpleExpr(expr.eval, expr.t))
  private def neg(expr: Expr): Expr      = ifNumeric(expr)(simple(Neg(_)))
  //private def not(expr: Expr): Expr      = ifNumeric(expr)(simple)
  private def compl(expr: Expr): Expr    = ifIntegral(expr)(simple(BitwiseNot(_)))
  //private def addrOf(expr: Expr): Expr   = ifNumeric(expr)(simple(Neg(_)))
  private def deref(expr: Expr): Expr    = PtrLval(ifPtr(expr)(simple(Deref)))
  private def chanRecv(expr: Expr): Expr = ifChan(expr)(simple(Deref))
}
