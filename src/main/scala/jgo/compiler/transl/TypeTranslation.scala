package jgo.compiler
package transl

import interm._
import codeseq._
import instr._
import symbol._
import types._

import RuntimeInfo._

trait TypeTranslation {
  def typeDesc(t: Type): String = t.radix match {
    case BoolType       => "Z"  //boolean
    case Int8  | Uint8  => "B"  //byte
    case Int16          => "S"  //short
    case Uint16         => "C"  //char
    case Int32 | Uint32 => "I"  //int
    case Int64 | Uint64 => "J"  //long
    case Float32        => "F"  //float
    case Float64        => "D"  //double
    
    case StringType     => "Ljava/lang/String;"
  }
  
  def methodDesc(f: Func): String = f.typeOf match {
    case FuncType(params, List(result), false) =>
      val sb = new StringBuilder
      sb append "("
      for (p <- params) sb append typeDesc(p)
      sb append ")"
      sb append typeDesc(result)
      sb.result
  }
}
