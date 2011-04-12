package jgo.compiler
package parser

import interm._
import types._

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
    | "*"  ~> unaryExpr  //^^# (ChanRecieve(_))
    | primaryExpr
    )
    
  lazy val exprList: P[List[Expr]] =               "expression list" $
    rep1sep(expression, ",")
}
