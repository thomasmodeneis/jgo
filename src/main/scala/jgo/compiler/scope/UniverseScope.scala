package jgo.compiler
package scope

import interm.types._

object UniverseScope extends Scope {
  private val bindings = imm.Map[String, Symbol] (
    "string"     -> new TypeName("string",     StringType),
    "bool"       -> new TypeName("bool",       Bool),
    "byte"       -> new TypeName("byte",       Uint8),
    "int"        -> new TypeName("int",        Int32),
    "uint"       -> new TypeName("uint",       Uint323),
    
    "uint8"      -> new TypeName("uint8",      Uint8),
    "uint16"     -> new TypeName("uint16",     Uint16),
    "uint32"     -> new TypeName("uint32",     Uint32),
    "uint64"     -> new TypeName("uint64",     Uint64),
    "int8"       -> new TypeName("int8",       Int8),
    "int16"      -> new TypeName("int16",      Int16),
    "int32"      -> new TypeName("int32",      Int32),
    "int64"      -> new TypeName("int64",      Int64),
    "float32"    -> new TypeName("float32",    Float32),
    "float64"    -> new TypeName("float64",    Float64),
    "complex64"  -> new TypeName("complex64",  Complex64),
    "complex128" -> new TypeName("complex128", Complex128)
    
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
  
  private implicit def pair2typeBinding(p: (String, Type)): (String, TypeSymbol) =
    (p._1, TypeSymbol(new TypeName(p._1, p._2)))
  
  def get(name: String)            = bindings get name
  def contains(name: String)       = bindings contains name
  def alreadyDefined(name: String) = contains(name)
}
