package jgo.compiler
package parser
package scoped

import message._
import scope._
import interm.symbol._
import interm.types._

trait Symbols extends Base with Scoped {
  
  lazy val onlyValueSymbol: P[ValueSymbol] = "var or const symbol" $
    ident ^? scope ^? { case v: ValueSymbol => v }
  
  lazy val onlyVarSymbol:   P[Variable] =             "var symbol" $
    ident ^? scope ^? { case v: Variable => v }
  
  lazy val onlyFuncSymbol:  P[Function] =             "var symbol" $
    ident ^? scope ^? { case v: Function => v }
  
  /*lazy val onlyConstSymbol: P[ConstSymbol] =      "const symbol" $
    ident ^? scope ^? { case c: ConstSymbol =? c } */
  
  lazy val onlyPkgSymbol:   P[Package] =          "package symbol" $
    ident ^? scope ^? { case p: Package => p }
  
  lazy val onlyTypeSymbol:  P[TypeSymbol] =          "type symbol" $
    ident ^? scope ^? { case t: TypeSymbol => t }
  
  
  lazy val symbol: PM[Symbol] =                           "symbol" $
    withPos(ident) ^^ (getSymbol _).tupled //Really, compiler?  Really?
  
  lazy val valueSymbol: PM[ValueSymbol] =
     withPos(symbol) ^^ { case (sM, p) =>
      sM flatMap {
        case v: ValueSymbol => Result(v)
        case s              => Problem("not a value: %s", s)(p)
      }
    }
  
  lazy val constSymbol: PM[ConstSymbol] =
    withPos(symbol) ^^ { case (sM, p) =>
      sM flatMap {
        case c: ConstSymbol => Result(c)
        case s              => Problem("not a constant: %s", s)(p)
      }
    }
  
  lazy val varSymbol: PM[Variable] =
    withPos(symbol) ^^ { case (sM, p) =>
      sM flatMap {
        case v: Variable => Result(v)
        case s           => Problem("not a variable: %s", s)(p)
      }
    }
  
  lazy val funcSymbol: PM[Function] =
    withPos(symbol) ^^ { case (sM, p) =>
      sM flatMap {
        case f: Function => Result(f)
        case s           => Problem("not a global function: %s", s)(p)
      }
    }
  
  lazy val pkgSymbol: PM[Package] =
    withPos(symbol) ^^ { case (sM, p) =>
      sM flatMap {
        case pkg: Package => Result(pkg)
        case s            => Problem("not a package: %s", s)(p)
      }
    }
  
  lazy val typeSymbol: PM[TypeSymbol] =
    withPos(symbol) ^^ { case (sM, p) =>
      sM flatMap {
        case t: TypeSymbol => Result(t)
        case s             => Problem("not a type: %s", s)(p)
      }
    }
  
  //not fully supported.
  lazy val qualifiedIdent: P_ =         "possibly qualified ident" $
    opt(ident <~ ".") ~ ident
  
  
  def getSymbol(name: String, p: Pos): M[Symbol] = scope.get(name) match {
    case Some(s) => Result(s)
    case None    => Problem("symbol not found: %s", name)(p)
  }
  
  def getVariable(name: String, p: Pos): M[Variable] = getSymbol(name, p) flatMap {
    case v: Variable => Result(v)
    case _           => Problem("not a variable: %s", name)(p)
  }
  
  def getTypeSymbol(name: String, p: Pos): M[TypeSymbol] = getSymbol(name, p) flatMap {
    case t: TypeSymbol => Result(t)
    case _             => Problem("not a type: %s", name)(p)
  }
}
