package jgo.compiler
package interm
package types

//import member._

/**
 * A named type which refers to some other type.
 * 
 * @todo Implement the following optimization: a wrapped type ''T'' whose
 *       referent is to be some other wrapped type ''R'' shall instead take
 *       its referent to be ''toAlias''(''R''), in a manner consistant with
 *       the type system.  Low priority.
 */
sealed trait NamedType extends Type {
  /** 
   * The name of this named type.
   */
  val name: String
  
  /** 
   * The type to which this named type refers.
   * We call that type the ''referent'' of this named type.
   */
  def referent: Type
  
  override def toString = name
}

/**
 * A named type that has no effective presence during execution; values of
 * this type are represented as values of the referent type at runtime.
 * 
 * We use type aliases instead of wrapped types whenever we can guarantee that
 * the named type in question will never have any methods.  This is the case for
 * the types declared the universe scope (the ''predeclared types'') and for
 * user-defined types declared in funcs.
 * 
 * @param name the name of this type alias
 * @param referent the referent of this type alias
 * 
 * @todo Implement a @GoSignature annotation that encodes requisite type
 *       information in class files (think: the `Signature` attribute),
 *       like Scala does.  High Priority, but deferred.
 */
class TypeAlias(val name: String, val referent: Type) extends NamedType {
  /**
   * The underlying type of this type alias, which is that of the referent.
   */
  def underlying = referent.underlying
  
  /**
   * The effective type of this type alias is the referent type, since
   * values of this alias type are represented as values of the referent
   * type.
   */
  def effective  = referent
  
  val semantics = underlying.semantics
}

/**
 * A named type that is reified at runtime.
 */
class WrappedType(val name: String, val referent: Type) extends NamedType {
  /**
   * The underlying type of this wrapped type, which is that of the referent.
   */
  def underlying = referent.underlying
  
  /**
   * The effective type of this wrapped type is this wrapped type itself,
   * since values of a wrapped type are stored as instances of a wrapper class
   * at runtime, not as instances of the referent type, as is the case with
   * type aliases.
   */
  def effective  = this
  
  val semantics = Reference  //!!!
}
