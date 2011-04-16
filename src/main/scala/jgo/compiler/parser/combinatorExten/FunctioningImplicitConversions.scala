package jgo.compiler.parser.combinatorExten

import scala.util.parsing.combinator.Parsers

trait FunctioningImplicitConversions {
  self: Parsers =>
  
  class Flattened2[+A, +B](p: Parser[A ~ B]) {
    def ^^ [R] (f: (A, B) => R): Parser[R] =
      p ^^ { case a ~ b => f(a, b) }
  }
  class Flattened3[+A, +B, +C](p: Parser[A ~ B ~ C]) {
    def ^^ [R] (f: (A, B, C) => R): Parser[R] =
      p ^^ { case a ~ b ~ c => f(a, b, c) }
  }
  class Flattened4[+A, +B, +C, +D](p: Parser[A ~ B ~ C ~ D]) {
    def ^^ [R] (f: (A, B, C, D) => R): Parser[R] =
      p ^^ { case a ~ b ~ c ~ d => f(a, b, c, d) }
  }
  class Flattened5[+A, +B, +C, +D, +E](p: Parser[A ~ B ~ C ~ D ~ E]) {
    def ^^ [R] (f: (A, B, C, D, E) => R): Parser[R] =
      p ^^ { case a ~ b ~ c ~ d ~ e => f(a, b, c, d, e) }
  }
  
  implicit def flat[A, B]         (p: Parser[A ~ B])             = new Flattened2(p)
  implicit def flat[A, B, C]      (p: Parser[A ~ B ~ C])         = new Flattened3(p)
  implicit def flat[A, B, C, D]   (p: Parser[A ~ B ~ C ~ D])     = new Flattened4(p)
  implicit def flat[A, B, C, D, E](p: Parser[A ~ B ~ C ~ D ~ E]) = new Flattened5(p)
}
