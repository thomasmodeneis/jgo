package jgo.compiler
package parser

import interm._
import types._
import codeseq._
import instr._

trait Expressions extends PrimaryExprs {
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
    | multExpr ~ ("<<" ~> unaryExpr)  //^^# conv{ BinExpr(op_<<, _, _) }
    | multExpr ~ (">>" ~> unaryExpr)  //^^# conv{ BinExpr(op_>>, _, _) }
    | multExpr ~ ("&"  ~> unaryExpr)  //^^# conv{ BinExpr(op_&, _, _)  }
    | multExpr ~ ("&^" ~> unaryExpr)  //^^# conv{ BinExpr(op_&^, _, _) }
    | unaryExpr
    )
  
  lazy val unaryExpr: PP[Expr] =          "unary expression: prec 6" $
    (("+"  ~> unaryExpr) //.&#
    | "-"  ~> unaryExpr  //^^# (Neg(_))
    | "!"  ~> unaryExpr  //^^# (Not(_))
    | "^"  ~> unaryExpr  //^^# (BitwiseNot(_))
    | "&"  ~> unaryExpr  //^^# (AddrOf(_))
    | "<-" ~> unaryExpr  //^^# (PtrDeref(_))
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
  
  
  def toStackType(t: NumericType): Arith = t match {
    case Uint8      => U8
    case Uint16     => U16
    case Uint32     => U32
    case Uint64     => U64
    case Int8       => I8
    case Int16      => I16
    case Int32      => I32
    case Int64      => I64
    case Float32    => F32
    case Float64    => F64
    case Complex64  => C64
    case Complex128 => C128
  }
  
  def shift(e1: Expr, e2: Expr): Option[Type] =
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
      Some(e1.t)
  
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
  
  def numeric(t: Type): Option[NumericType] = t.underlying match {
    case nt: NumericType => Some(nt)
    case _ =>
      val s = String.format("operand type %s not numeric", t)
      recordErr(s)
      None
  }
  
  def integral(t: Type): Option[IntegralType] = t.underlying match {
    case it: IntegralType => Some(it)
    case _ =>
      val s = String.format("operand type %s not integral", t)
      recordErr(s)
      None
  }
  
  def unsigned(t: Type): Option[UnsignedType] = t.underlying match {
    case ut: UnsignedType => Some(ut)
    case _ =>
      val s = String.format("operand type %s not unsigned", t)
      recordErr(s)
      None
  }
}
