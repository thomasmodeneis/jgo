package jgo.compiler
package interm
package symbols

//import interm.types._
//import interm.Variable

trait Symbol

/*
sealed abstract class PossiblyUnresolvedSymbol
//  case class Resolved(s: Symbol) extends PossiblyUnresolvedSymbol
  case class Unresloved(req: String)(res: Symbol => PossiblyUnresolvedSymbol)
  extends PossiblyUnresolvedSymbol

sealed abstract class Symbol(val name: String) extends PossiblyUnresolvedSymbol
  
  case class TypeSymbol(t: TypeName) extends Symbol(t.name)
  
  case class PkgSymbol(name: String) extends Symbol(name)
  
  sealed abstract class ValueSymbol(val name: String, val typeOf: Type) extends Symbol(name)
    
    case class VarSymbol(
      name:   String,
      id:     Variable,
      typeOf: Type)
    extends ValueSymbol(name, typeOf)
    
    case class ConstSymbol(
      name:   String,
      value:  Option[Any],
      typeOf: Type)
    extends ValueSymbol(name, typeOf)
*/
