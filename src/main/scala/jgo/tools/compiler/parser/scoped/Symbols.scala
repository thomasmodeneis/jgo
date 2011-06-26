package jgo.tools.compiler
package parser
package scoped

import scope._
import interm.symbol._
import interm.types._
import interm.expr.BuiltinFuncExpr

trait Symbols extends Base with Scoped {
  
  lazy val onlyValueSymbol: Parser[ValueSymbol] = "var or const symbol" $
    ident ^? scope ^? { case v: ValueSymbol => v }
  
  lazy val onlyVarSymbol:   Parser[Variable] =             "var symbol" $
    ident ^? scope ^? { case v: Variable => v }
  
  lazy val onlyFuncSymbol:  Parser[Function] =             "var symbol" $
    ident ^? scope ^? { case v: Function => v }
  
  lazy val onlyConstSymbol: Parser[ConstSymbol] =        "const symbol" $
    ident ^? scope ^? { case c: ConstSymbol => c }
  
  lazy val onlyPkgSymbol:   Parser[Package] =          "package symbol" $
    ident ^? scope ^? { case p: Package => p }
  
  lazy val onlyTypeSymbol:  Parser[TypeSymbol] =          "type symbol" $
    ident ^? scope ^? { case t: TypeSymbol => t }
  
  lazy val onlyBfuncSymbol: Parser[BuiltinFuncExpr] =    "builtin func" $
    ident ^? scope ^? { case BuiltinFuncSymbol(b) => b }
  
  
  lazy val symbol: Rule[Symbol] =                              "symbol" $
    withPos(ident) ^^ (getSymbol _).tupled //Really, compiler?  Really?
  
  lazy val valueSymbol: Rule[ValueSymbol] =
     withPos(symbol) ^^ { case (sErr, p) =>
      sErr flatMap {
        case v: ValueSymbol => result(v)
        case s              => problem("not a value: %s", s)(p)
      }
    }
  
  lazy val constSymbol: Rule[ConstSymbol] =
    withPos(symbol) ^^ { case (sErr, p) =>
      sErr flatMap {
        case c: ConstSymbol => result(c)
        case s              => problem("not a constant: %s", s)(p)
      }
    }
  
  lazy val varSymbol: Rule[Variable] =
    withPos(symbol) ^^ { case (sErr, p) =>
      sErr flatMap {
        case v: Variable => result(v)
        case s           => problem("not a variable: %s", s)(p)
      }
    }
  
  lazy val funcSymbol: Rule[Function] =
    withPos(symbol) ^^ { case (sErr, p) =>
      sErr flatMap {
        case f: Function => result(f)
        case s           => problem("not a global function: %s", s)(p)
      }
    }
  
  lazy val pkgSymbol: Rule[Package] =
    withPos(symbol) ^^ { case (sErr, p) =>
      sErr flatMap {
        case pkg: Package => result(pkg)
        case s            => problem("not a package: %s", s)(p)
      }
    }
  
  lazy val typeSymbol: Rule[TypeSymbol] =
    withPos(symbol) ^^ { case (sErr, p) =>
      sErr flatMap {
        case t: TypeSymbol => result(t)
        case s             => problem("not a type: %s", s)(p)
      }
    }
  
  //not fully supported.
  lazy val qualifiedIdent: P_ =         "possibly qualified ident" $
    opt(ident <~ ".") ~ ident
  
  
  def getSymbol(name: String, p: Pos): Err[Symbol] =
    Err.fromOption(scope.get(name))("symbol not found: %s", name)(p)
  
  def getVariable(name: String, p: Pos): Err[Variable] = getSymbol(name, p) flatMap {
    case v: Variable => result(v)
    case _           => problem("not a variable: %s", name)(p)
  }
  
  def getTypeSymbol(name: String, p: Pos): Err[TypeSymbol] = getSymbol(name, p) flatMap {
    case t: TypeSymbol => result(t)
    case _             => problem("not a type: %s", name)(p)
  }
}
