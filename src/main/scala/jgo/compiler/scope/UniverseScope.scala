package jgo.compiler
package scope

import interm.types._
import interm.symbols._

object UniverseScope extends Scope {
  private implicit def t(p: (String, Type)): (String, TypeSymbol) =
    (p._1, TypeSymbol(new TypeName(p._1, p._2)))
  
  private val bindings = Map[String, Symbol] (
    t("string"     -> StringType),
    t("bool"       -> Bool),
    t("byte"       -> Uint8),
    t("int"        -> Int32),
    t("uint"       -> Uint32),
    
    t("uint8"      -> Uint8),
    t("uint16"     -> Uint16),
    t("uint32"     -> Uint32),
    t("uint64"     -> Uint64),
    t("int8"       -> Int8),
    t("int16"      -> Int16),
    t("int32"      -> Int32),
    t("int64"      -> Int64),
    t("float32"    -> Float32),
    t("float64"    -> Float64),
    t("complex64"  -> Complex64),
    t("complex128" -> Complex128)
    
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
  
  def get(name: String)            = bindings get name
  def contains(name: String)       = bindings contains name
  def alreadyDefined(name: String) = contains(name)
}
