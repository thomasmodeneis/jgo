package jgo.compiler
package transl

import interm._
import codeseq._
import instr._
import symbol._
import types._

import RuntimeInfo._

import org.objectweb.asm.{Label => AsmLabel, Type => AsmType, Opcodes}
import Opcodes._
import AsmType._

trait TypeResolution {
  implicit def class2asmType(cl: Class[_]) = AsmType.getType(cl)
  //implicit def desc2asmType(desc: String)  = AsmType.getType(desc)
  
  def stackSize(t: Type): Int = t.effective match {
    case Int64 | Uint64 | Float64 => 2
    case _                        => 1
  }
  
  def stackSize(t: StackType): Int = t match {
    case I64 | U64 | F64 => 2
    case _               => 1
  }
  
  def toAsmType(t: Type): AsmType = t.effective match {
    case BoolType       => BOOLEAN_TYPE
    case Int8  | Uint8  => BYTE_TYPE
    case Int16          => SHORT_TYPE
    case Uint16         => CHAR_TYPE
    case Int32 | Uint32 => INT_TYPE
    case Int64 | Uint64 => LONG_TYPE
    case Float32        => FLOAT_TYPE
    case Float64        => DOUBLE_TYPE
    
    case StringType => AsmType.getObjectType("java/lang/String")
    
    case SliceType(t) => AsmType.getObjectType(SliceClass)
    
    //not sure why this shouldn't be the rule for everything
    case tEff => AsmType.getType(typeDesc(tEff))
  }
  
  def toAsmType(t: StackType): AsmType = t match {
    case Bool      => BOOLEAN_TYPE
    case I8  | U8  => BYTE_TYPE
    case I16       => SHORT_TYPE
    case U16       => CHAR_TYPE
    case I32 | U32 => INT_TYPE
    case I64 | U64 => LONG_TYPE
    case F32       => FLOAT_TYPE
    case F64       => DOUBLE_TYPE
    
    case Obj => AsmType.getObjectType("java/lang/Object") //refine? not with StackType
  }
  
  def typeDesc(t: Type): String = t.effective match {
    case BoolType       => "Z"  //boolean
    case Int8  | Uint8  => "B"  //byte
    case Int16          => "S"  //short
    case Uint16         => "C"  //char
    case Int32 | Uint32 => "I"  //int
    case Int64 | Uint64 => "J"  //long
    case Float32        => "F"  //float
    case Float64        => "D"  //double
    
    case StringType     => "Ljava/lang/String;"
    
    //note: O(n^2) time, where n is the dimension of the array type
    case ArrayType(len, t) => "[" + typeDesc(t)
    
    case SliceType(t) => SliceDesc
  }
  
  def typeDesc(t: StackType): String = t match {
    case Bool      => "Z"  //boolean
    case I8  | U8  => "B"  //byte
    case I16       => "S"  //short
    case U16       => "C"  //char
    case I32 | U32 => "I"  //int
    case I64 | U64 => "J"  //long
    case F32       => "F"  //float
    case F64       => "D"  //double
    
    case Obj => "Ljava/lang/Object;"
  }
  
  def methodDesc(f: Func): String = f.typeOf match {
    case FuncType(params, results, false) =>
      val paramStr = params map typeDesc mkString ("(", "", ")")
      results match {
        case Nil          => paramStr + "V"
        case List(result) => paramStr + typeDesc(result)
      }
  }
}
