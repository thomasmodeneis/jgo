import jgo.compiler._
import parser.BlockLang
import parser.combinatorExten.TracePrintingParsers
import lexer.Scanner

object BlockTest {
  def main(args: Array[String]) {
    test("{ }")
    test("{ ; }")
    test("{ { } }")
    test("{ { }; { } }")
    test("{ var x int }")
    test("""
{
  var x, y int
  var s1, s2 string
  var a [10]int
  x, y = y, x
  s1 = s1 + s2
  x, y = x / y, x % y
  var u uint
  x << u
  x >> u
}""")
  }
  
  def test(in: String) {
    try {
      println("testing: " + in)
      
      val sc = Scanner(in)
      print("tokenization: ")
      sc foreach { t => print(t + " ") }
      println()
      
      val bl = new BlockLang(sc) //with TracePrintingParsers
      println(bl.errors)
      println(bl.result map { _.listing })
    }
    catch {
      case e =>
        println("exception: " + e)
        e.printStackTrace()
    }
  }
}
