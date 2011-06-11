package jgo.tools.compiler
package transl
package func

import interm._
import instr._

case class UnsupportedInstrException(i: Instr) extends UnsupportedOperationException(i.toString)
