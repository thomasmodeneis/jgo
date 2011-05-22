package jgo.compiler
package parser

import scala.util.parsing.input.Reader

import lexer._
import scope._
import interm._
import interm.types._

import stmts._
import funcs._

class BlockLang(in: Reader[Token], res: List[Type] = Nil, resNamed: Boolean = false) extends FuncContext with Statements {
  //def, not val.  See comment in StackScoped
  def initialEnclosing = UniverseScope
  
  def targetFuncType = FuncType(Nil, res)
  def hasNamedResults = resNamed
  
  lazy val result = phrase(block)(in)
}

object BlockLang {
  import java.io.{File, InputStream, FileInputStream, InputStreamReader}
  import scala.collection.immutable.PagedSeq
  
  def apply(in: Reader[Char]):  BlockLang = new BlockLang(Scanner(in))
  def apply(inStr: String):     BlockLang = new BlockLang(Scanner(inStr))
  def apply(in: InputStream):   BlockLang = new BlockLang(Scanner(in))
  def apply(file: File):        BlockLang = new BlockLang(Scanner(file))
  
  def from(fileName: String):   BlockLang = new BlockLang(Scanner.from(fileName))
}
