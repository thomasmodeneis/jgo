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

object Signature {
  def results(ps: List[ParamVar], rs: List[ParamVar], isVari: Boolean, hasNmRes: Boolean) =
    new Signature {
      val params  = ps
      val results = rs
      val isVariadic = isVari
      val hasNamedResults = hasNmRes
      lazy val paramTypes  = params  map { _.t }
      lazy val resultTypes = results map { _.t }
    }
  
  def singleUnnamedResult(ps: List[ParamVar], rT: Type, isVari: Boolean) =
    new Signature {
      val params  = ps
      val results = List(new DummyVar(rT))
      lazy val paramTypes = params map { _.t }
      val resultTypes = List(rT)
      val isVariadic = isVari
      def hasNamedResults = false
    }
  
  def noResults(ps: List[ParamVar], isVari: Boolean) =
    new Signature {
      val params = ps
      lazy val paramTypes = params map { _.t }
      def results = Nil
      def resultTypes = Nil
      val isVariadic = isVari
      def hasNamedResults = false
      override def isVoid = true
    }
}
