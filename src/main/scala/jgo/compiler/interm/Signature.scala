package jgo.compiler
package interm

import interm._
import interm.types._
import symbol._

sealed abstract class Signature extends Typed {
  val typeOf = FuncType(paramTypes, resultTypes, isVariadic)
  
  def paramTypes:  List[Type]
  def resultTypes: List[Type]
  def isVariadic:  Boolean
  
  def namedParams:  List[LocalVar]
  def namedResults: List[LocalVar]
  
  def numParams  = paramTypes.length
  def numResults = resultTypes.length
  
  def hasNamedResults: Boolean
  
  def isVoid = resultTypes.length == 0
}

object Signature {
  def results(ps: List[ParamVar], rs: List[ParamVar], isVari: Boolean, hasNmRes: Boolean) =
    new Signature {
      val namedParams  = ps collect { case p: LocalVar => p }
      val namedResults = rs collect { case r: LocalVar => r }
      val isVariadic = isVari
      val hasNamedResults = hasNmRes
      lazy val paramTypes  = ps map { _.t }
      lazy val resultTypes = rs map { _.t }
    }
  
  def singleResult(ps: List[ParamVar], rT: Type, isVari: Boolean) =
    new Signature {
      val namedParams  = ps collect { case p: LocalVar => p }
      def namedResults = Nil
      val paramTypes  = ps map { _.t }
      val resultTypes = List(rT)
      val isVariadic = isVari
      def hasNamedResults = false
      override def isVoid = false
    }
  
  def noResults(ps: List[ParamVar], isVari: Boolean) =
    new Signature {
      val namedParams = ps collect { case p: LocalVar => p }
      val paramTypes  = ps map { _.t }
      def namedResults = Nil
      def resultTypes  = Nil
      val isVariadic = isVari
      def hasNamedResults = false
      override def isVoid = true
    }
}
