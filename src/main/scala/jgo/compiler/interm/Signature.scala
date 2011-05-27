package jgo.compiler
package interm

import interm._
import interm.types._
import symbol._

sealed abstract class Signature extends Typed {
  lazy val typeOf = FuncType(paramTypes, resultTypes, isVariadic)
  
  def paramTypes:  List[Type]
  def resultTypes: List[Type]
  def isVariadic:  Boolean
  
  def namedParams:  List[LocalVar]
  def namedResults: List[LocalVar]
  
  def numParams  = paramTypes.length
  def numResults = resultTypes.length
  
  def hasNamedResults: Boolean
  
  def isVoid = resultTypes.length == 0
  
  override def toString =
    "Signature(paramTypes = " + paramTypes + ", resultTypes = " + resultTypes +
    (if (isVariadic) ", variadic)" else ")")
}

object Signature {
  def results(ps: List[ParamVar], rs: List[ParamVar], isVari: Boolean, hasNmRes: Boolean) =
    new Signature {
      val namedParams  = ps collect { case p: LocalVar => p }
      val namedResults = rs collect { case r: LocalVar => r }
      val isVariadic = isVari
      val hasNamedResults = hasNmRes
      val paramTypes  = ps map { _.typeOf }
      val resultTypes = rs map { _.typeOf }
    }
  
  def singleResult(ps: List[ParamVar], rT: Type, isVari: Boolean) =
    new Signature {
      val namedParams  = ps collect { case p: LocalVar => p }
      def namedResults = Nil
      val paramTypes  = ps map { _.typeOf}
      val resultTypes = List(rT)
      val isVariadic = isVari
      
      def hasNamedResults = false
      override def isVoid = false
    }
  
  def noResults(ps: List[ParamVar], isVari: Boolean) =
    new Signature {
      val namedParams = ps collect { case p: LocalVar => p }
      val paramTypes  = ps map { _.typeOf }
      
      def namedResults = Nil
      def resultTypes  = Nil
      
      val isVariadic = isVari
      
      def hasNamedResults = false
      override def isVoid = true
    }
}
