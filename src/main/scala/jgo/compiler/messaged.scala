package jgo.compiler

package object messaged {
  /**
   * A result, or a list error messages, both possibly with warnings
   * and notes.  This type is declared here as "Messaged", but is called
   * "M" everywhere else so as to make signatures more readable.
   */
  sealed abstract class Messaged[+T] {
    def get:       T
    def isDefined: Boolean
    
    def errors:   List[ErrorMsg]
    
    def getOrElse[U >: T](substitute:  U): U
    def orElse   [U >: T](alternative: Messaged[U]): Messaged[U]
    
    def asOption: Option[T]
    def asEither: Either[List[ErrorMsg], T]
    
    def foreach (f: T => Any): Unit
    
    def map     [T2] (f: T => T2):           Messaged[T2]
    def flatMap [T2] (f: T => Messaged[T2]): Messaged[T2]
    
    def then[T2](m: Messaged[T2]): Messaged[T2]
    
    def filter(p: T => Boolean): Messaged[T]
    
    def apply[A, R](a: A)(implicit ev: T <:< (A => R)): Messaged[R] =
      for (v <- this)
      yield (ev(v))(a)
    
    def apply[A, B, R](a: A, b: B)(implicit ev: T <:< ((A, B) => R)): Messaged[R] =
      for (v <- this)
      yield (ev(v))(a, b)
    
    def apply[A, B, C, R](a: A, b: B, c: C)(implicit ev: T <:< ((A, B, C) => R)): Messaged[R] =
      for (v <- this)
      yield (ev(v))(a, b, c)
    
    def apply[A, B, C, D, R](a: A, b: B, c: C, d: D)(implicit ev: T <:< ((A, B, C, D) => R)): Messaged[R] =
      for (v <- this)
      yield (ev(v))(a, b, c, d)
    
    private[messaged] val e: List[ErrorMsg]
  }
  
  case class Result[+T](result: T) extends Messaged[T] {
    def get       = result
    def isDefined = true
    
    def errors = Nil
    
    def getOrElse[U >: T](subst: U)         = result
    def orElse   [U >: T](alt: Messaged[U]) = this
    
    def asOption = Some(result)
    def asEither = Right(result)
    
    def foreach(f: T => Any) = f(result)
    
    def map    [T2](f: T => T2)           = new Result(f(result))
    def flatMap[T2](f: T => Messaged[T2]) = f(result)
    
    def then [T2](m: Messaged[T2]) = m
    def after[T2](m: Messaged[T2]) = m then this
    
    def filter(p: T => Boolean) =
      if (p(result)) this
      else new Problem(Nil)
    
    private[messaged] val e = Nil
  }
  
  class Problem private[messaged](private[messaged] val e: List[ErrorMsg]) extends Messaged[Nothing] {
    def get       = throw new NoSuchElementException
    def isDefined = false
    
    lazy val errors   = e reverse
    
    def getOrElse[U](subst: U)         = subst
    def orElse   [U](alt: Messaged[U]) = alt
    
    def asOption = None
    def asEither = Left(errors)
    
    def foreach(f: Nothing => Any) = ()
    
    def map    [T2](f: Nothing => T2)           = this
    def flatMap[T2](f: Nothing => Messaged[T2]) = this
    
    def then[T2](m: Messaged[T2]) = new Problem(m.e ::: e)
    
    def filter(p: Nothing => Boolean) = this
  }
  
  object Messaged {
    def apply[T](t: T): Messaged[T] = new Result(t)
    
    implicit def lsM2mLs[T](ms: List[Messaged[T]]): Messaged[List[T]] =
      if (ms forall { _.isDefined }) {
        var rs: List[T]          = Nil
        for (m <- ms) {
          val res = m.asInstanceOf[Result[T]]
          rs ::= res.result
        }
        new Result(rs.reverse)
      }
      else {
        var es: List[ErrorMsg]   = Nil
        for (m <- ms)
          es = m.e ::: es
        new Problem(es)
      }
    
    implicit def optM2mOpt[T](opt: Option[Messaged[T]]): Messaged[Option[T]] = opt match {
      case Some(r @ Result(result)) => new Result(Some(result))
      case Some(p: Problem) => p
      case None => new Result(None)
    }
    
    
    def together[A, B](a: Messaged[A], b: Messaged[B]): Messaged[(A, B)] = (a, b) match {
      case (r1: Result[_], r2: Result[_]) => new Result((r1.result, r2.result))
      case _ => new Problem(e(a, b))
    }
    
    def together[A, B, C](a: Messaged[A], b: Messaged[B], c: Messaged[C]): Messaged[(A, B, C)] = (a, b, c) match {
      case (r1: Result[_], r2: Result[_], r3: Result[_]) => new Result((r1.result, r2.result, r3.result))
      case _ => new Problem(e(a, b, c))
    }
    
    def together[A, B, C, D](a: Messaged[A], b: Messaged[B], c: Messaged[C], d: Messaged[D]): Messaged[(A, B, C, D)] = {
      val collected = a then b then c then d
      (a, b, c, d) match {
        case (Result(r1), Result(r2), Result(r3), Result(r4)) => collected then ((r1, r2, r3, r4))
        case _ => collected.asInstanceOf[Problem] //Yeah... invariants are invariants...
      }
    }
    
    def together[A, B, C, D, E](a: Messaged[A], b: Messaged[B], c: Messaged[C], d: Messaged[D], e: Messaged[E]): Messaged[(A, B, C, D, E)] = {
      val collected = a then b then c then d then e
      (a, b, c, d, e) match {
        case (Result(r1), Result(r2), Result(r3), Result(r4), Result(r5)) => collected then ((r1, r2, r3, r4, r5))
        case _ => collected.asInstanceOf[Problem] //and hackish solutions are hackish solutions.
      }
    }
    
    @inline private def e[A, B](a: Messaged[A], b: Messaged[B]) = b.e ::: a.e
    @inline private def e[A, B, C](a: Messaged[A], b: Messaged[B], c: Messaged[C]) = c.e ::: b.e ::: a.e
  }
  
  object Problem {
    def apply(msg: String, args: Any*)(implicit pos: Pos) =
      new Problem(List(ErrorMsg(msg.format(args: _*)).setPos(pos)))
    
    def unapply(m: Messaged[_]) = m match {
      case p: Problem => Some(p.errors)
      case _ => None
    }
  }
}
