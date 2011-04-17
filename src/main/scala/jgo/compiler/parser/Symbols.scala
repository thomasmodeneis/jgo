package jgo.compiler
package parser

import scope._
import interm.symbols._
import interm.types._

trait Symbols extends Base with Scoped {
  
  lazy val valueSymbol: P[ValueSymbol] = "var or const symbol" $
    ident ^? scope ^? { case v: ValueSymbol => v }
  
  lazy val varSymbol:   P[Variable] =             "var symbol" $
    ident ^? scope ^? { case v: Variable => v }
  
  lazy val funcSymbol:  P[Function] =            "var symbol" $
    ident ^? scope ^? { case v: Function => v }
  
  /*lazy val constSymbol: P[ConstSymbol] =        "const symbol" $
    ident ^? symbols.constProj*/
  
  lazy val pkgSymbol:   P[Package] =          "package symbol" $
    ident ^? scope ^? { case p: Package => p }
  
  lazy val typeSymbol:  P[TypeSymbol] =          "type symbol" $
    ident ^? scope ^? { case t: TypeSymbol => t }
  
  
  lazy val symbol: P[Symbol] =                        "symbol" $
    ident ^^ { str => scope.get(str) match {
      case Some(s) => s
      case None    => recordErr("symbol not found"); NoSymbol
    }}
  
  //not fully supported.
  lazy val qualifiedIdent: P_ =     "possibly qualified ident" $
    opt(ident <~ ".") ~ ident
}
