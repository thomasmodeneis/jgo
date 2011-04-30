package jgo.compiler
package parser
package scoped

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
  
  /*lazy val constSymbol: P[ConstSymbol] =          "const symbol" $
    ident ^? symbols.constProj*/
  
  lazy val onlyPkgSymbol:   P[Package] =          "package symbol" $
    ident ^? scope ^? { case p: Package => p }
  
  lazy val onlyTypeSymbol:  P[TypeSymbol] =          "type symbol" $
    ident ^? scope ^? { case t: TypeSymbol => t }
  
  
  lazy val symbol: P[Option[Symbol]] =                    "symbol" $
    ident ^^ getSymbol
  
  //not fully supported.
  lazy val qualifiedIdent: P_ =         "possibly qualified ident" $
    opt(ident <~ ".") ~ ident
  
  
  def getSymbol(name: String): Option[Symbol] = scope.get(name) match {
    case Some(s) => Some(s)
    case None    => badSymbol("symbol not found: %s", name)
  }
  
  def getVariable(name: String): Option[Variable] = getSymbol(name) match {
    case Some(v: Variable) => Some(v)
    case Some(_)           => badSymbol("not a variable symbol: %s", name)
    case None              => None
  }
  
  def getTypeSymbol(name: String): Option[TypeSymbol] = getSymbol(name) match {
    case Some(t: TypeSymbol) => Some(t)
    case Some(_)             => badSymbol("not a type symbol: %s", name)
    case None                => None
  }
  
  def badSymbol(msg: String, args: AnyRef*): None.type = {
    recordErr(msg, args: _*)
    None
  }
}
