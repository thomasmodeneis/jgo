package jgo.compiler
package scope

import interm._
import interm.types._
import interm.symbol._

object UniverseScope extends MapScope {
  private implicit def t(p: (String, Type)): (String, TypeSymbol) =
    (p._1, TypeSymbol(new TypeName(p._1, p._2)))
  
  private implicit def toTypeSymbol(t: Type with Named) = TypeSymbol(t)
  
  protected val bindings = Map[String, Symbol] (
    "string"     -> StringType,
    "bool"       -> BoolType,
    "byte"       -> Uint8,
    "int"        -> IntType,
    "uint"       -> UintType,
    
    "uint8"      -> Uint8,
    "uint16"     -> Uint16,
    "uint32"     -> Uint32,
    "uint64"     -> Uint64,
    "int8"       -> Int8,
    "int16"      -> Int16,
    "int32"      -> Int32,
    "int64"      -> Int64,
    "float32"    -> Float32,
    "float64"    -> Float64,
    "complex64"  -> Complex64,
    "complex128" -> Complex128
    
    /*
    "uintptr" ->
    */
    
    /*
    "true"  ->
    "false" ->
    "iota"  ->
    "nil"   ->
    */
    
    /*
    "append"
    "cap"
    "close"
    "closed"
    "complex"
    "copy"
    "imag"
    "len"
    "make"
    "new"
    "panic"
    "print"
    "println"
    "real"
    "recover"
    */
  )
}
