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
  
  override def toString = {
    val paramStr =
      if (!isVariadic) "func(" + params.mkString(", ") + ")"
      else "func(" + params.init.mkString(", ") + ", ..." + params.tail + ")"
    
    val resultStr = results match {
      case List()  => ""
      case List(r) => " " + r.toString
      case rs      => rs.mkString(" (", ", ", ")")
    }
    
    paramStr + resultStr
  }
}
