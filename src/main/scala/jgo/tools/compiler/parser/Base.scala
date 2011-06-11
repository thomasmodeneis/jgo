package jgo.tools.compiler
package parser

//import lexer.{Scanner, Lexical}
import scope._

import combinatorExten._

import scala.util.parsing._
import combinator._
import input.Reader
import input.NoPosition

trait Base extends Tokens with PackratParsers with FancyParsers with MessageHandling {
  
  type Rule[+T]   = Parser[Err[T]]
  type LrRule[+T] = PackratParser[Err[T]]
  
  @deprecated("Implement this grammar rule.", "May 29, 2011")
  type P_      = Parser       [Any]
  @deprecated("Implement this grammar production, being sure to use LrRule.", "May 29, 2011")
  type PP_     = PackratParser[Any]
  
  
  implicit def string2Fancy(str: String) = new FancyParserOps(str)
  
  /**
   * We override this method because its implementation in PackratParsers
   * assumes that all input is of type PackratReader and therefore performs a cast
   * 
   * @todo Better integrate concept of applying a parser to a prefix of input,
   *       converting input as necessary, probably as a method of Rule, to be
   *       implemented in a new combinatorExten.MessageHandling 
   */
  override def memo[T](p: Parser[T]): PackratParser[T] =
    new PackratParser[T] {
      def apply(in: Input): ParseResult[T] = in match {
        case _: PackratReader[_] => Base.super.memo[T](p)(in)
        case _                   => Base.super.memo[T](p)(new PackratReader(in))
      }
    }
  
  
  final def catchSyntaxErr[T](p: Parser[Err[T]]): Parser[Err[T]] =
    Parser { in => injectSyntaxErr(p(in)) }
  
  final def catchSyntaxErr[T](msg: String, p: Parser[Err[T]]): Parser[Err[T]] =
    Parser { in => injectSyntaxErr(p(in), msg) }
  
  final def extractFromParseResult[T](vErrR: ParseResult[Err[T]]): Err[T] = vErrR match {
    case Success(vErr, in)  => vErr
    case NoSuccess(msg, in) => problem(msg)(in.pos)
  }
  
  final def injectSyntaxErr[T](vErrR: ParseResult[Err[T]]): ParseResult[Err[T]] = vErrR match {
    case s: Success[_]  => s
    case f: Failure     => f
    case Error(msg, in) => Success(problem(msg)(in.pos), in)
  }
  
  final def injectSyntaxErr[T](vErrR: ParseResult[Err[T]], msg: String): ParseResult[Err[T]] = vErrR match {
    case s: Success[_] => s
    case f: Failure    => f
    case Error(_, in)  => Success(problem(msg)(in.pos), in)
  }
  
  
  //implicit def wrapResult[T](v: T): Err[T] = Err(v)
  implicit def liftList[T](errs:  List[Err[T]]): Err[List[T]]   = Err.liftList(errs)
  implicit def liftOpt [T](opt: Option[Err[T]]): Err[Option[T]] = Err.liftOpt(opt)
  
  implicit def parserConv[A, A1](p: Parser[A])(implicit ev: A => A1): Parser[A1] =
    p ^^ ev
  
  implicit def leftConv[A, A1, B](v: A ~ B)(implicit ev: A => A1): A1 ~ B = v match {
    case a ~ b => new ~(ev(a), b)
  }
  
  implicit def rightConv[A, B, B1](v: A ~ B)(implicit ev: B => B1): A ~ B1 = v match {
    case a ~ b => new ~(a, ev(b))
  }
  
  implicit def pWrapResult[T](p: Parser[T]): Parser[Err[T]] = p ^^ result
  
  implicit def pLiftList[T](p: Parser[List[Err[T]]]): Parser[Err[List[T]]] =
    p ^^ Err.liftList
  
  implicit def pLiftOpt[T](p: Parser[Option[Err[T]]]): Parser[Err[Option[T]]] =
    p ^^ Err.liftOpt
  
  
  lazy val identList: Parser[List[String]] =                    "identifier list" $
    rep1sep(ident, ",")
  
  lazy val identPosList: Parser[List[(String, Pos)]] =      "ident-with-pos list" $
    rep1sep(withPos(ident), ",")
  
  def repWithSemi[T](p: Parser[T]): Parser[List[T]] =
    ("repetition of "
    + p
    + " with semicolon; last semicolon optional"
    ) $
    repsep(p, ";") <~ ";".?
}