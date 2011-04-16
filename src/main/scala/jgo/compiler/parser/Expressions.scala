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
    ( addExpr ~ ("+" ~> multExpr)  //^^# conv{ BinExpr(op_+, _, _) }
    | addExpr ~ ("-" ~> multExpr)  //^^# conv{ BinExpr(op_-, _, _) }
    | addExpr ~ ("|" ~> multExpr)  //^^# conv{ BinExpr(op_|, _, _) }
    | addExpr ~ ("^" ~> multExpr)  //^^# conv{ BinExpr(op_^, _, _) }
    | multExpr
    )
  
  lazy val multExpr: PP[Expr] =  "multiplicative expression: prec 5" $
    ( multExpr ~ ("*"  ~> unaryExpr)  //^^# conv{ BinExpr(op_*, _, _)  }
    | multExpr ~ ("/"  ~> unaryExpr)  //^^# conv{ BinExpr(op_/, _, _)  }
    | multExpr ~ ("%"  ~> unaryExpr)  //^^# conv{ BinExpr(op_%, _, _)  }
    | multExpr ~ ("<<" ~> unaryExpr)  ^^ {
        case e ~ f => shift(e, f) match {
          case Some((t, i, u)) => SimpleExpr(e.eval |+| f.eval |+| ShiftL(i, u), t)
          case None            => ExprError
        }
      }
    | multExpr ~ (">>" ~> unaryExpr)  ^^ {
        case e ~ f => shift(e, f) match {
          case Some((t, i, u)) => SimpleExpr(e.eval |+| f.eval |+| ShiftR(i, u), t)
          case None            => ExprError
        }
      }
    | multExpr ~ ("&"  ~> unaryExpr)  //^^# conv{ BinExpr(op_&, _, _)  }
    | multExpr ~ ("&^" ~> unaryExpr)  //^^# conv{ BinExpr(op_&^, _, _) }
    | unaryExpr
    )
  
  lazy val unaryExpr: PP[Expr] =          "unary expression: prec 6" $
    (("+"  ~> unaryExpr) ^^ { ifNumeric(_) { (e, t) => e } }
    | "-"  ~> unaryExpr  ^^ { ifNumeric(_) { (e, t) => SimpleExpr(e.eval |+| Neg(t), e.t) } }//{ e => arith(e.t) match { case Some(st) => SimpleExpr(e.t, e.eval |+| Neg(st)); case None => ExprError } }
    | "!"  ~> unaryExpr  //^^# (Not(_))
    | "^"  ~> unaryExpr  ^^ { ifIntegral(_) { (e, t) => SimpleExpr(e.eval |+| BitwiseNot(t), e.t) } }
    | "&"  ~> unaryExpr  //^^# (AddrOf(_))
    | "<-" ~> unaryExpr  //^^ { case e if e.t.underlying 
    | "*"  ~> unaryExpr  ^^ {
        e =>
        e.t.underlying match {
          case PointerType(t) =>
            PtrLval(e)
          case _ =>
            ExprError
        }
      }
    | primaryExpr
    )
    
  lazy val exprList: P[List[Expr]] =               "expression list" $
    rep1sep(expression, ",")
  
  
  /*
    if (!e1.t.underlying.isInstanceOf[Integral]) {
      val s = String.format("type, %s, of left operand of shift not integral", e1.t)
      recordErr(s)
      None
    }
    else if (!e2.t.underlying.isInstanceOf[Unsigned]) {
      val s = String.format("type, %s, of right operand of shift not unsigned", e2.t)
      recordErr(s)
      None
    }
    else
      Some((e1.t, toArith(e1.t.underlying.asInstanceOf[Integral]), toArith(e1.t.underlying.asInstanceOf[Unsigned])))
  */
  
  def sameType(e1: Expr, e2: Expr): Option[Type] =
    if (e1.t == e2.t)
      Some(e1.t)
    else {
      val s = String.format("operands have differing types %s and %s", e1.t, e2.t)
      recordErr(s)
      None
    }
  
  def addable(t: Type): Option[AddableType] = t.underlying match {
    case at: AddableType => Some(at)
    case _ =>
      val s = String.format("operand type %s not numeric or string type", t)
      recordErr(s)
      None
  }
  
  def arith(t: Type): Option[Arith] = t.underlying match {
    case nt: NumericType => Some(nt)
    case _ =>
      val s = String.format("operand type %s not numeric", t)
      recordErr(s)
      None
  }
  
  def integral(t: Type): Option[Integral] = t.underlying match {
    case it: IntegralType => Some(it)
    case _ =>
      val s = String.format("operand type %s not integral", t)
      recordErr(s)
      None
  }
  
  def unsigned(t: Type): Option[Unsigned] = t.underlying match {
    case ut: UnsignedType => Some(ut)
    case _ =>
      val s = String.format("operand type %s not unsigned", t)
      recordErr(s)
      None
  }
  
  def sameAddable(e1: Expr, e2: Expr):  Option[AddableType] = sameType(e1, e2) flatMap addable
  def sameArith(e1: Expr, e2: Expr):    Option[Arith]       = sameType(e1, e2) flatMap arith
  def sameIntegral(e1: Expr, e2: Expr): Option[Integral]    = sameType(e1, e2) flatMap integral
  def sameUnsigned(e1: Expr, e2: Expr): Option[Unsigned]    = sameType(e1, e2) flatMap unsigned
}
