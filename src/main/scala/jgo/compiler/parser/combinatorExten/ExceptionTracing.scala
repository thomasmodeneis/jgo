package jgo.compiler
package parser.combinatorExten

trait ExceptionTracing extends FancyParsers {
  override def nameize[T](p: Parser[T], name: String) =
    exceptWrap(super.nameize(p, name), name)
  
  def exceptWrap[T](p: Parser[T], name: String): Parser[T] = Parser { in =>
    try p(in)
    catch {
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
