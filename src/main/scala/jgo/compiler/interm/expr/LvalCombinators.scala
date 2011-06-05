package jgo.compiler
package interm
package expr

import types._
import instr._
import instr.TypeConversions._
import codeseq._

import Utils._

trait LvalCombinators extends Combinators with TypeChecks {
  private def lval(e: Expr, desc: String)(pos: Pos): M[LvalExpr] = e match {
    case l: LvalExpr => Result(l)
    case _ => Problem("lvalue expected for %s", desc)(pos)
  }
  
  private def mkPtr(e: Expr, desc: String)(pos: Pos): M[Expr] =
    if (e.addressable) Result(e.mkPtr)
    else Problem("addressable expression expected for %s", desc)(pos)
  
  
  def addrOf(e: Expr)(pos: Pos): M[Expr] = for {
    p <- mkPtr(e, "operand")(pos)
  } yield p
  
  def deref(e: Expr)(pos: Pos): M[Expr] = for {
    res <- e match {
      case HasType(PointerType(elemT)) => Result(PtrDerefLval(e, elemT))
      case _ =>
        Problem("operand of pointer indirection/dereference has type %s; pointer type required", e.typeOf)(pos)
    }
  } yield res
  
  //def select(obj: Expr, selector: String)(pos: Pos): M[Expr]
  
  
  private def mkIndex(arr: Expr, indx: Expr)(pos: Pos) = {
    @inline def isIntegral = indx.typeOf.underlying.isInstanceOf[IntegralType]
    arr match {
      case HasType(ArrayType(_, elemT)) =>
        if (isIntegral) Result(ArrayIndexLval(arr, indx, elemT))
        else Problem("index type %s is inappropriate for an array; integral type required", indx.typeOf)(pos)
      case HasType(SliceType(elemT)) =>
        if (isIntegral) Result(SliceIndexLval(arr, indx, elemT))
        else Problem("index type %s is inappropriate for a slice; integral type required", indx.typeOf)(pos)
      case HasType(StringType) =>
        if (isIntegral) Result(EvalExpr(arr.eval |+| indx.evalUnder |+| StrIndex, scope.UniverseScope.byte))
        else Problem("index type %s is inappropriate for a string; integral type required", indx.typeOf)(pos)
      case HasType(MapType(keyT, valT)) =>
        if (keyT <<= indx.typeOf) Result(MapIndexLval(arr, indx, valT))
        else Problem(
          "index type %s is inappropriate for a map of type %s; must be assignable to key type %s",
          indx.typeOf, arr.typeOf, keyT)(pos)
    }
  }
  
  def index(arr: Expr, indx: Expr)(pos: Pos): M[Expr] =
    for (result <- mkIndex(arr, indx)(pos))
    yield result
  
  def slice(arr: Expr, low: Option[Expr], high: Option[Expr])(pos: Pos): M[Expr] =
    for {
      _ <- (
        for (l <- low)  yield integral(l, "lower bound of slice")(pos), //these Option[M[Expr]]'s are implicitly
        for (h <- high) yield integral(h, "upper bound of slice")(pos)  //converted to M[Option[Expr]]'s
      )
      (boundsStackingCode, boundsInfo) = (low, high) match {
        case (Some(e1), Some(e2)) => (e1.evalUnder |+| e2.evalUnder, BothBounds)
        case (Some(e1), None)     => (e1.evalUnder,                  LowBound)
        case (None,     Some(e2)) => (                 e2.evalUnder, HighBound)
        case (None,     None)     => (CodeBuilder(),                 NoBounds)
      }
      stackingCode = arr.evalUnder |+| boundsStackingCode
      result: Expr <- arr match {
        case HasType(ArrayType(_, t)) => Result(EvalExpr(stackingCode |+| SliceArray(t, boundsInfo), SliceType(t)))
        case HasType(SliceType(t))    => Result(EvalExpr(stackingCode |+| SliceSlice(t, boundsInfo), SliceType(t)))
        case HasType(StringType)      => Result(EvalExpr(stackingCode |+| Substring(boundsInfo),     StringType))
        case _ => Problem("cannot slice a value of type %s; array, slice, or string type required", arr.typeOf)(pos)
      }
    } yield result
  
  def incr(e: Expr)(pos: Pos): M[CodeBuilder] = for {
    l <- lval(e, "operand of ++")(pos)
    intT <- integral(l, "operand of ++")(pos) //could tuple together for indep. errors, but why bother?
  } yield l match {
    case VarLval(vr) if vr.typeOf.effective == vr.typeOf.underlying => Incr(vr, 1, intT)
    case _ =>
      val storeEval = UnderlyingExpr(l.loadUnder |+| PushInt(1, intT) |+| Add(intT), l.typeOf).eval
      l.store(storeEval)
  }
  
  def decr(e: Expr)(pos: Pos): M[CodeBuilder] = for {
    l <- lval(e, "operand of --")(pos)
    intT <- integral(l, "operand of --")(pos)
  } yield l match {
    case VarLval(vr) if vr.typeOf.effective == vr.typeOf.underlying => Decr(vr, 1, intT)
    case _ =>
      val storeEval = UnderlyingExpr(l.loadUnder |+| PushInt(1, intT) |+| Sub(intT), l.typeOf).eval
      l.store(storeEval)
  }
  
  
  
  private def lvalues(es: List[Expr], desc: String)(pos: Pos): M[List[LvalExpr]] =
    for ((e, i) <- es.zipWithIndex)
    yield for (l <- lval(e, "%s term of %s" format (ordinal(i + 1), desc))(pos))
    yield l //the implicit conversion Messaged.lsM2mLs lifts that List[M[LvalExpr]] to M[List[..]]
  
  
  private def zipAndCheckArity[A](as: List[A], bs: List[Expr])(pos: Pos): M[List[(A, Expr)]] = {
    var res: List[(A, Expr)] = Nil
    var (curA, curB) = (as, bs)
    var lenA, lenB = 0
    while (!curA.isEmpty || !curB.isEmpty) {
      if (curA isEmpty) {
        lenB += curB.length
        return Problem("arity (%d) of left side of assignment unequal to arity (%d) of right side", lenA, lenB)(pos)
      }
      if (curB isEmpty) {
        lenA += curA.length
        return Problem("arity (%d) of left side of assignment unequal to arity (%d) of right side", lenA, lenB)(pos)
      }
      val pair = (curA.head, curB.head)
      curA = curA.tail
      curB = curB.tail
      res = pair :: res
    }
    Result(res reverse)
  }
  
  private def checkAssignAndConvert(e: Expr, t: Type)(pos: Pos): M[Expr] =
    convertForAssign(e, t, "right side")(pos) //"right side has type %s not assignable..."
  
  private def checkAssignAndConvert(ets: List[(Expr, Type)])(pos: Pos): M[List[Expr]] = 
    if (ets.length == 1) {
      val (e, t) = ets(0) //ets := plural of (e, t), so to speak. That's where the name comes from.
      checkAssignAndConvert(e, t)(pos) map (_ :: Nil)
    } else {
      val res: M[List[Expr]] = //conversion!
        for (((e, t), i) <- ets zipWithIndex)
        yield convertForAssign(e, t, "%s expression on right side" format ordinal(i + 1))(pos)
      res
    }
  
  def assign(lefts0: List[Expr], rights0: List[Expr])(pos: Pos): M[CodeBuilder] =
    for {
      lefts <- lvalues(lefts0, "left side of assignment")(pos)
      rls <- zipAndCheckArity(rights0, lefts)(pos)
      rts = rls map { case (r, l) => (r, l.typeOf) }
      rights <- checkAssignAndConvert(rts)(pos)
      pairs = lefts zip rights
    } yield {
      val (leftCode, rightCode) = (pairs foldLeft (CodeBuilder.empty, CodeBuilder.empty)) {
        //TODO: Add code that converts r.eval to the appropriate type
        case ((leftAcc, rightAcc), (l, r)) => (leftAcc |+| l.storePrefix(r.eval), l.storeSuffix |+| rightAcc)
      }
      leftCode |+| rightCode
    }
  
  def assign(left0: Expr, right0: Expr)(pos: Pos): M[CodeBuilder] =
    for ((left, right) <- (lval(left0, "left side of assignment")(pos), checkAssignAndConvert(right0, left0.typeOf)(pos)))
    yield left.store(right.eval)
}
