package jgo.compiler.parser.combinatorExten

import scala.util.parsing.combinator.Parsers

trait TracePrintingParsers extends FancyParsers {
  override def nameize[T](p: Parser[T], name: String) =
    log(super.nameize(p, name))(name)
  
  override def log[T](p: => Parser[T])(name: String): Parser[T] = Parser{ in =>
    println("trying "+ name +" at\n"+
      in.pos.longString.linesWithSeparators.map("| " + _).mkString)
    val r = p(in)
    println(name +" --> "+ r)
    r
  }
}