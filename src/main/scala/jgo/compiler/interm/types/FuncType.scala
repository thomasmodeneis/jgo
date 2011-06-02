package jgo.compiler
package interm
package types

case class FuncType(params: List[Type], results: List[Type], isVariadic: Boolean = false) extends UnderType with NilableType {
  /**
   * not sure what is existentially appropriate here,
   * but this has the correct codegen implications because
   * copy constructor should should not be called on
   * assignment of function values.
   */
  val semantics = Reference
  
  override def toString =
    if (!isVariadic)
      "func(" + params.mkString(", ") + ") " + results.mkString(", ")
    else
      //ugh...
      "func(" + params.init.mkString(", ") + ", ..." + params.tail + ") " + results.mkString(", ")
}
