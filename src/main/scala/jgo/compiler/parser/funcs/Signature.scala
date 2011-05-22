package jgo.compiler
package parser
package funcs

import interm._
import interm.types._
import symbol._

sealed abstract class Signature extends Typed {
  val typeOf = FuncType(paramTypes, resultTypes, isVariadic)
  
  def paramTypes:  List[Type]
  def resultTypes: List[Type]
  def isVariadic:  Boolean
  
  def params:  List[ParamVar]
  def results: List[ParamVar]
  
  def numParams  = paramTypes.length
  def numResults = resultTypes.length
  
  def hasNamedResults: Boolean
  
  def isVoid = results.length == 0
}

class NamedResultSignature(
    val params:  List[ParamVar],
    val results: List[ParamVar],
    val isVariadic: Boolean)
extends Signature {
  lazy val paramTypes  = params  map { _.t }
  lazy val resultTypes = results map { _.t }
  
  def hasNamedResults = true
}

class UnnamedResultSignature(
    val params: List[ParamVar],
    val resultTypes: List[Type],
    val isVariadic: Boolean)
extends Signature {
  lazy val paramTypes = params map { _.t }
  lazy val results = resultTypes map { t => new DummyVar(t) }
  
  def hasNamedResults = false
}

class VoidSignature(
    val params: List[ParamVar],
    val isVariadic: Boolean)
extends Signature {
  lazy val paramTypes = params map { _.t }
  
  def results = Nil
  def resultTypes = Nil
  
  def hasNamedResults = false
  
  override def isVoid = true
}
