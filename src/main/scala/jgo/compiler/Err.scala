package jgo.compiler

/**
 * A result, or a list of compilation errors.
 */
sealed abstract class Err[+T] {
  def get:       T
  def isDefined: Boolean
  
  def errors: List[ErrorMsg]
  
  def getOrElse[U >: T](substitute:  U): U
  def orElse   [U >: T](alternative: Err[U]): Err[U]
  
  def asOption: Option[T]
  def asEither: Either[List[ErrorMsg], T]
  
  def foreach(f: T => Any): Unit
  
  def map     [T2] (f: T => T2):      Err[T2]
  def flatMap [T2] (f: T => Err[T2]): Err[T2]
  
  def then[T2](m: Err[T2]): Err[T2]
  
  def filter(p: T => Boolean): Err[T]
  
  def apply[A, R](a: A)(implicit ev: T <:< (A => R)): Err[R] =
    for (v <- this)
    yield (ev(v))(a)
  
  def apply[A, B, R](a: A, b: B)(implicit ev: T <:< ((A, B) => R)): Err[R] =
    for (v <- this)
    yield (ev(v))(a, b)
  
  def apply[A, B, C, R](a: A, b: B, c: C)(implicit ev: T <:< ((A, B, C) => R)): Err[R] =
    for (v <- this)
    yield (ev(v))(a, b, c)
  
  def apply[A, B, C, D, R](a: A, b: B, c: C, d: D)(implicit ev: T <:< ((A, B, C, D) => R)): Err[R] =
    for (v <- this)
    yield (ev(v))(a, b, c, d)
  
  /** This is meant to be a member private to Err, Result, and Problems! */
  def es: List[ErrorMsg]
}

case class Result[+T](result: T) extends Err[T] {
  def get       = result
  def isDefined = true
  
  def errors = Nil
  
  def getOrElse[U >: T](subst: U)    = result
  def orElse   [U >: T](alt: Err[U]) = this
  
  def asOption = Some(result)
  def asEither = Right(result)
  
  def foreach(f: T => Any) = f(result)
  
  def map    [T2](f: T => T2)      = new Result(f(result))
  def flatMap[T2](f: T => Err[T2]) = f(result)
  
  def then [T2](m: Err[T2]) = m
  
  def filter(p: T => Boolean) =
    if (p(result)) this
    else Problems.fromBackwardsList(Nil) //not really meaningful, but required
  
  def es = Nil
}

class Problems private (val es: List[ErrorMsg]) extends Err[Nothing] {
  def get       = throw new NoSuchElementException
  def isDefined = false
  
  lazy val errors = es.reverse
  
  def getOrElse[U](subst: U)    = subst
  def orElse   [U](alt: Err[U]) = alt
  
  def asOption = None
  def asEither = Left(errors)
  
  def foreach(f: Nothing => Any) = ()
  
  def map    [T2](f: Nothing => T2)      = this
  def flatMap[T2](f: Nothing => Err[T2]) = this
  
  def then[T2](m: Err[T2]) = new Problems(m.es ::: es)
  
  def filter(p: Nothing => Boolean) = this
}

object Err {
  def apply[T](t: T): Err[T] = new Result(t)
  
  def fromOption[T](opt: Option[T])(msg: String, args: Any*)(implicit pos: Pos) = opt match {
    case Some(v) => Result(v)
    case None    => Problems.one(msg, args: _*)(pos)
  }
  
  implicit def liftList[T](ms: List[Err[T]]): Err[List[T]] =
    if (ms forall (_.isDefined)) {
      var rs: List[T]  = Nil
      for (m <- ms) {
        val res = m.asInstanceOf[Result[T]]
        rs ::= res.result
      }
      new Result(rs.reverse)
    }
    else {
      var es: List[ErrorMsg] = Nil
      for (m <- ms)
        es = m.es ::: es
      Problems.fromBackwardsList(es)
    }
  
  implicit def liftOpt[T](opt: Option[Err[T]]): Err[Option[T]] = opt match {
    case Some(r @ Result(result)) => new Result(Some(result))
    case Some(p: Problems) => p
    case None => new Result(None)
  }
  
  
  def together[A, B](a: Err[A], b: Err[B]): Err[(A, B)] = (a, b) match {
    case (r1: Result[_], r2: Result[_]) => new Result((r1.result, r2.result))
    case _ => Problems.fromBackwardsList(e(a, b))
  }
  
  def together[A, B, C](a: Err[A], b: Err[B], c: Err[C]): Err[(A, B, C)] = (a, b, c) match {
    case (r1: Result[_], r2: Result[_], r3: Result[_]) => new Result((r1.result, r2.result, r3.result))
    case _ => Problems.fromBackwardsList(e(a, b, c))
  }
  
  def together[A, B, C, D](a: Err[A], b: Err[B], c: Err[C], d: Err[D]): Err[(A, B, C, D)] = {
    val collected = a then b then c then d
    (a, b, c, d) match {
      case (Result(r1), Result(r2), Result(r3), Result(r4)) => collected then Result(r1, r2, r3, r4)
      case _ => collected.asInstanceOf[Problems] //Yeah... invariants are invariants...
    }
  }
  
  def together[A, B, C, D, E](a: Err[A], b: Err[B], c: Err[C], d: Err[D], e: Err[E]): Err[(A, B, C, D, E)] = {
    val collected = a then b then c then d then e
    (a, b, c, d, e) match {
      case (Result(r1), Result(r2), Result(r3), Result(r4), Result(r5)) => collected then Result(r1, r2, r3, r4, r5)
      case _ => collected.asInstanceOf[Problems] //and hackish solutions are hackish solutions.
    }
  }
  
  @inline private def e[A, B](a: Err[A], b: Err[B]) = b.es ::: a.es
  @inline private def e[A, B, C](a: Err[A], b: Err[B], c: Err[C]) = c.es ::: b.es ::: a.es
}

object Problems {
  def one(msg: String, args: Any*)(implicit pos: Pos) =
    new Problems(List(ErrorMsg(msg.format(args: _*), pos)))
  
  def fromBackwardsList(es: List[ErrorMsg]) =
    new Problems(es)
  
  def unapply(m: Err[_]) = m match {
    case p: Problems => Some(p.errors)
    case _ => None
  }
}
