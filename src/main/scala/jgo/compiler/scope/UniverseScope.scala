package jgo.compiler
package scope

import interm._
import types._
import symbol._
import expr._

object UniverseScope extends MapScope {
  private implicit def t(p: (String, Type)): (String, TypeSymbol) =
    (p._1, TypeSymbol(new TypeAlias(p._1, p._2)))
  
  //private implicit def toTypeSymbol(t: Type) = TypeSymbol(new TypeAlias(t))
  
  protected val bindings = Map[String, Symbol] (
    "string"     -> new TypeAlias("string", StringType),
    "bool"       -> new TypeAlias("bool",   BoolType),
    "byte"       -> new TypeAlias("byte",   Uint8),
    "int"        -> new TypeAlias("int",    Int32),
    "uint"       -> new TypeAlias("uint",   Uint32),
    
    "uint8"      -> new TypeAlias("uint8",  Uint8),
    "uint16"     -> new TypeAlias("uint16", Uint16),
    "uint32"     -> new TypeAlias("uint32", Uint32),
    "uint64"     -> new TypeAlias("uint64", Uint64),
    "int8"       -> new TypeAlias("int8",   Int8),
    "int16"      -> new TypeAlias("int16",  Int16),
    "int32"      -> new TypeAlias("int32",  Int32),
    "int64"      -> new TypeAlias("int64",  Int64),
    "float32"    -> new TypeAlias("float32", Float32),
    "float64"    -> new TypeAlias("float64", Float64),
    "complex64"  -> new TypeAlias("complex64",  Complex64),
    "complex128" -> new TypeAlias("complex128", Complex128),
    
    /*
    "uintptr" ->
    */
    
    "true"  -> new ConstSymbol(BoolConst(true)),
    "false" -> new ConstSymbol(BoolConst(false)),
    "iota"  -> IotaSymbol,
    "nil"   -> new ConstSymbol(NilConst)
    
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
