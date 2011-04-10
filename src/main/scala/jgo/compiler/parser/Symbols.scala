package jgo.compiler
package parser

import scope._
import interm.symbols._
import interm.types._

trait Symbols extends Base {
  def symbols: Scope
  
  lazy val valueSymbol: P[ValueSymbol] = "var or const symbol" $
    ident ^? symbols ^? { case v: ValueSymbol => v }
  
  lazy val varSymbol:   P[Variable]   =           "var symbol" $
    ident ^? symbols ^? { case v: Variable => v }
  
  /*lazy val constSymbol: P[ConstSymbol] =        "const symbol" $
    ident ^? symbols.constProj*/
  
  lazy val pkgSymbol:   P[Package]   =        "package symbol" $
    ident ^? symbols ^? { case p: Package => p }
  
  lazy val typeSymbol:  P[TypeSymbol]  =         "type symbol" $
    ident ^? symbols ^? { case t: TypeSymbol => t }
  
  
  lazy val symbol: P[Symbol] =                        "symbol" $
    ident ^^ { str => symbols.get(str) match {
      case Some(s) => s
      case None    => throw new NoSuchElementException("symbol not found; apologies for the poor error handling")
    }}
  
  //not fully supported.
  lazy val qualifiedIdent: P_ =     "possibly qualified ident" $
    opt(ident <~ ".") ~ ident
}
