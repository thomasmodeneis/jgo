package jgo.tools.compiler
package transl

import interm._
import codeseq._
import instr._
import symbol._
import types._
import interm.Label
import interm.types.Type

import scope.UniverseScope

import RuntimeInfo._

trait GoSignatures {
  def typeSig(t: Type): String = t match {
    case wt: WrappedType => "L%s;" format (wt.name) //add package info
    
    case UniverseScope.bool => "b"
    
    case UniverseScope.int  => "i0"
    case UniverseScope.uint => "u0"
    
    case UniverseScope.int8  => "i1"
    case UniverseScope.int16 => "i2"
    case UniverseScope.int32 => "i4"
    case UniverseScope.int64 => "i8"
    
    case UniverseScope.uint8  => "u1"
    case UniverseScope.uint16 => "u2"
    case UniverseScope.uint32 => "u4"
    case UniverseScope.uint64 => "u8"
    
    case UniverseScope.float32 => "f4"
    case UniverseScope.float64 => "f8"
    
    case UniverseScope.complex64  => "x4"
    case UniverseScope.complex128 => "x8"
    
    case UniverseScope.string => "s"
    
    case ArrayType(len, t) => "[%d:%s" format (len, typeSig(t))
    
    case SliceType(t) => "[" + typeSig(t)
    
    case MapType(k, v) => "M<" + typeSig(k) + "," + typeSig(v) + ">"
    
    case PointerType(t) => "*" + typeSig(t)
    
    case BidirChanType(t) => "C"  + typeSig(t)
    case SendChanType(t)  => "CS" + typeSig(t)
    case RecvChanType(t)  => "CR" + typeSig(t)
    
    case FuncType(ps, rs, false) => "F("  + sigsOf(ps).mkString + ")(" + sigsOf(rs).mkString + ")"
    case FuncType(ps, rs, true)  => "FV(" + sigsOf(ps).mkString + ")(" + sigsOf(rs).mkString + ")"
  }
  
  private def sigsOf(ts: List[Type]): List[String] =
    ts map typeSig
  
  //def functionSignature(f: Function): String =
    //f.name + "(" + f.typeOf.params.map(typeSignature).mkString + 
}
