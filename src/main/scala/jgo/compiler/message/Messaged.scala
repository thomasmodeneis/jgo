package jgo.compiler
package message

import scala.util.parsing.input.Position

sealed abstract class Messaged[+T] {
  def get:       T
  def isDefined: Boolean
  
  def errors:   List[ErrorMsg]
  def warnings: List[WarningMsg]
  def notes:    List[NoteMsg]
  
  def getOrElse[U >: T](substitute:  U):           U
  def orElse   [U >: T](alternative: Messaged[U]): Messaged[U]
  
  def asOption: Option[T]
  def asEither: Either[List[ErrorMsg], T]
  
  def foreach (f: T => Any): Unit
  
  def map     [T2] (f: T => T2):           Messaged[T2]
  def flatMap [T2] (f: T => Messaged[T2]): Messaged[T2]
  
  def filter(p: T => Boolean): Messaged[T]
  
  /**
   * Performs a monadic bind, just like flatMap, not a functoric (?) map. 
   * Never use this function directly, lest you (and I) look like a moron.
   * This blatant hack permits us to use for expressions much more readily with Messaged.
   * Witness:
   * {{{
   *  def neg(eM: Messaged[Expr]) =
   *    for (e <- eM)
   *    yield e match {
   *      case HasType(t: NumericType) => SimpleExpr(e.eval |+| Neg(t), e.t) //Make use of an implicit conv.
   *      case _ => Problem("operand of - (negation) is of type %s; expected numeric type", e.t)
   *    }
   * }}}
   * I'll get rid of this abomination as soon as I find a cleaner way of accomplishing that.
   */
  //final def map[M, T2](f: T => M)(implicit ev: M <:< Messaged[T2]): Messaged[T2] =
    //flatMap(ev compose f)
  
  private[message] val e: List[ErrorMsg]
  private[message] val w: List[WarningMsg]
  private[message] val n: List[NoteMsg]
  private[message] def wnAppendedTo(w: List[WarningMsg], n: List[NoteMsg]): Messaged[T]
}

class Result[+T] private[message](
  val result: T,
  private[message] val w: List[WarningMsg],
  private[message] val n: List[NoteMsg])
extends Messaged[T] {
  def get       = result
  def isDefined = true
  
  def      errors   = Nil
  lazy val warnings = w reverse
  lazy val notes    = n reverse
  
  def getOrElse[U >: T](subst: U)         = result
  def orElse   [U >: T](alt: Messaged[U]) = this
  
  def asOption = Some(result)
  def asEither = Right(result)
  
  def foreach(f: T => Any) = f(result)
  
  def map    [T2](f: T => T2)           = new Result(f(result), w, n)
  def flatMap[T2](f: T => Messaged[T2]) = f(result).wnAppendedTo(w, n)
  
  def filter(p: T => Boolean) =
    if (p(result))
      this
    else
      new Problem(Nil, w, n)
  
  private[message] val e = Nil
  private[message] def wnAppendedTo(w0: List[WarningMsg], n0: List[NoteMsg]) =
    new Result(result, w ::: w0, n ::: n0)
}

class Problem private[message](
  private[message] val e: List[ErrorMsg],
  private[message] val w: List[WarningMsg],
  private[message] val n: List[NoteMsg])
extends Messaged[Nothing] {
  def get       = throw new NoSuchElementException
  def isDefined = false
  
  lazy val errors   = e reverse
  lazy val warnings = w reverse
  lazy val notes    = n reverse
  
  def getOrElse[U](subst: U)         = subst
  def orElse   [U](alt: Messaged[U]) = alt
  
  def asOption = None
  def asEither = Left(errors)
  
  def foreach(f: Nothing => Any) = ()
  
  def map    [T2](f: Nothing => T2)           = this
  def flatMap[T2](f: Nothing => Messaged[T2]) = this
  
  def filter(p: Nothing => Boolean) = this
  
  private[message] def wnAppendedTo(w0: List[WarningMsg], n0: List[NoteMsg]) =
    new Problem(e, w ::: w0, n ::: n0)
}

object Messaged {
  implicit def res2Msgd[T](t: T): Messaged[T] = new Result(t, Nil, Nil)
  
  implicit def lsM2mLs[T](ms: List[Messaged[T]]): Messaged[List[T]] =
    if (ms forall { _.isDefined }) {
      var rs: List[T]          = Nil
      var ws: List[WarningMsg] = Nil
      var ns: List[NoteMsg]    = Nil
      for (m <- ms) {
        val res = m.asInstanceOf[Result[T]]
        rs ::= res.result
        //ws = res.w ::: ws
        //ns = res.n ::: ns
      }
      new Result(rs.reverse, ws, ns)
    }
    else {
      var es: List[ErrorMsg]   = Nil
      var ws: List[WarningMsg] = Nil
      var ns: List[NoteMsg]    = Nil
      for (m <- ms) {
        es = m.e ::: es
        //ws = m.w ::: ws
        //ns = m.n ::: ns
      }
      new Problem(es, ws, ns)
    }
  
  implicit def optM2mOpt[T](opt: Option[Messaged[T]]): Messaged[Option[T]] = (opt: @unchecked) match {
    case Some(r: Result[T]) => new Result(Some(r.result), r.w, r.n)
    case Some(p: Problem)   => p
    case None => new Result(None, Nil, Nil)
  }
  
  
  def together[A, B](a: Messaged[A], b: Messaged[B]): Messaged[(A, B)] = (a, b) match {
    case (r1: Result[_], r2: Result[_]) => new Result((r1.result, r2.result), w(r1, r2), n(r1, r2))
    case _ => new Problem(e(a, b), w(a, b), n(a, b))
  }
  
  def together[A, B, C](a: Messaged[A], b: Messaged[B], c: Messaged[C]): Messaged[(A, B, C)] = (a, b, c) match {
    case (r1: Result[_], r2: Result[_], r3: Result[_]) => new Result((r1.result, r2.result, r3.result), w(r1, r2, r3), n(r1, r2, r3))
    case _ => new Problem(e(a, b, c), w(a, b, c), n(a, b, c))
  }
  
  @inline private def e[A, B](a: Messaged[A], b: Messaged[B]) = b.e ::: a.e
  @inline private def w[A, B](a: Messaged[A], b: Messaged[B]) = b.w ::: a.w
  @inline private def n[A, B](a: Messaged[A], b: Messaged[B]) = b.n ::: a.n
  
  @inline private def e[A, B, C](a: Messaged[A], b: Messaged[B], c: Messaged[C]) = c.e ::: b.e ::: a.e
  @inline private def w[A, B, C](a: Messaged[A], b: Messaged[B], c: Messaged[C]) = c.w ::: b.w ::: a.w
  @inline private def n[A, B, C](a: Messaged[A], b: Messaged[B], c: Messaged[C]) = c.n ::: b.n ::: a.n
}

object Result {
  //def apply[T](r: T, w: List[WarningMsg], n: List[NoteMsg]) = new Result(r, w, n)
  def apply[T](r: T) = new Result(r, Nil, Nil)
  
  def unapply[T](m: Messaged[T]) = m match {
    case r: Result[_] => Some(r.result, r.warnings, r.notes)
    case _ => None
  }
}

object Problem {
  //def apply(e: List[ErrorMsg], w: List[WarningMsg], n: List[NoteMsg]) = new Problem(e, w, n)
  //def apply(e: List[ErrorMsg]) = new Problem(e, Nil, Nil)
  
  def apply(msg: String, args: Any*)(implicit pos: Position) =
    new Problem(List(ErrorMsg(msg.format(args: _*)).setPos(pos)), Nil, Nil)
  
  def unapply(m: Messaged[_]) = m match {
    case p: Problem => Some(p.errors, p.warnings, p.notes)
    case _ => None
  }
}

