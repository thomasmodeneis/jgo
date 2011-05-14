import jgo.compiler._
import parser.BlockLang
import parser.combinatorExten._
import lexer._

import message._
import interm.codeseq._

object LexComment {
  def main(args: Array[String]) {
    test("")
    test("/*hello*/")
    test("//hello")
    test("//hello\n")
    test("abc\n//hello")
    test("abc\n//hello\n")
    test("abc\n//hello\nabc")
    test("abc/*hello*/")
    test("abc/*hello*/abc")
    test("abc\n/*hello*/")
    test("abc\n/*hello*/abc")
    test("abc/*hello*/\n")
    test("abc/*hello*/\nabc")
    test("abc\n/*hello*/\n")
    test("abc\n/*hello*/\nabc")
  }
  
  def test(in: String) {
    println("testing: [" + in + "]")
    println()
    
    var cur = Scanner(in)
    print("tokenization: ")
    while (!cur.atEnd) {
      print(cur.first + " ")
      cur = cur.rest
    }
    println()
    println()
  }
}
