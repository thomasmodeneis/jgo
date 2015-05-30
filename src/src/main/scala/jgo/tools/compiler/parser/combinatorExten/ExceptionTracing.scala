package jgo.tools.compiler
package parser.combinatorExten

trait ExceptionTracing extends FancyParsers {
  override def nameize[T](p: Parser[T], name: String) =
    exceptWrap(super.nameize(p, name), name)
  
  def exceptWrap[T](p: Parser[T], name: String): Parser[T] = Parser { in =>
    try {
      assert(in != null, "null input in " + name)
      val r = p(in)
      assert(r != null, "null result in " + name)
      r
    } catch {
      case e =>
        println(in.pos.longString)
        println("AT: " + name)
        println("EX: " + e)
        println()
        println()
        throw e
    }
  }
}
