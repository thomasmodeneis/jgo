package jgo.tools.compiler
package transl
package func

import interm._
import instr._

case class AtInstrException(i: Instr, e: Throwable) extends RuntimeException(i.toString + ", " + e.toString)
