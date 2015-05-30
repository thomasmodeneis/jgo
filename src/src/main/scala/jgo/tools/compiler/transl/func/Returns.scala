package jgo.tools.compiler
package transl
package func

import interm._
import instr._

trait Returns extends FuncTranslBase {
  protected override def translateInstr(i: Instr): Unit = i match {
    case Return      => gen.returnValue() //hopefully, this works.
    case ValueReturn => gen.returnValue()
    
    case _ => super.translateInstr(i)
  }
}