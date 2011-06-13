package jgo.tools.compiler
package scope

import interm._
import types._
import symbol._
import expr._

object UniverseScope extends MapScope {
  private implicit def t(p: (String, Type)): (String, TypeSymbol) =
    (p._1, TypeSymbol(new TypeAlias(p._1, p._2)))
  
  //private implicit def toTypeSymbol(t: Type) = TypeSymbol(new TypeAlias(t))
  
  val string = new TypeAlias("string", StringType)
  val bool   = new TypeAlias("bool",   BoolType)
  val byte   = uint8
  val int    = new TypeAlias("int",    Int32)
  val uint   = new TypeAlias("uint",   Uint32)
  
  val uint8  = new TypeAlias("uint8",  Uint8)
  val uint16 = new TypeAlias("uint16", Uint16)
  val uint32 = new TypeAlias("uint32", Uint32)
  val uint64 = new TypeAlias("uint64", Uint64)
  val int8   = new TypeAlias("int8",   Int8)
  val int16  = new TypeAlias("int16",  Int16)
  val int32  = new TypeAlias("int32",  Int32)
  val int64  = new TypeAlias("int64",  Int64)
  
  val float32 = new TypeAlias("float32", Float32)
  val float64 = new TypeAlias("float64", Float64)
  
  val complex64  = new TypeAlias("complex64",  Complex64)
  val complex128 = new TypeAlias("complex128", Complex128)
  
  val True  = new ConstSymbol(UntypedBoolConst(true))
  val False = new ConstSymbol(UntypedBoolConst(false))
  
  val iota = IotaSymbol
  val nil  = new ConstSymbol(NilConst)
  
  val len = BuiltinFuncSymbol(bfunc.Len)
  
  protected val bindings = Map[String, Symbol] (
    "string" -> string,
    "bool"   -> bool,
    "byte"   -> byte,
    "int"    -> int,
    "uint"   -> uint,
    
    "uint8"  -> uint8,
    "uint16" -> uint16,
    "uint32" -> uint32,
    "uint64" -> uint64,
    "int8"   -> int8,
    "int16"  -> int16,
    "int32"  -> int32,
    "int64"  -> int64,
    
    "float32" -> float32,
    "float64" -> float64,
    
    "complex64"  -> complex64,
    "complex128" -> complex128,
    
    /*
    "uintptr" -> //I don't think I'm going to support this.
    */
    
    "true"  -> True,
    "false" -> False,
    "iota"  -> iota,
    "nil"   -> nil,
    
    "len" -> len
    
    /*
    "append"
    "cap"
    "close"
    "closed" //this has since been removed from the spec
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
