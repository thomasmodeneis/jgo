package jgo.tools.compiler
package interm
package expr
package combin

import types._
import instr._
import instr.TypeConversions._
import codeseq._

import Utils._

trait LvalCombinators extends Combinators with TypeChecks {
  private def lval(e: Expr, desc: String)(pos: Pos): Err[LvalExpr] = e match {
    case l: LvalExpr => result(l)
    case _ => problem("lvalue expected for %s", desc)(pos)
  }
  
  private def mkPtr(e: Expr, desc: String)(pos: Pos): Err[Expr] =
    if (e.addressable) result(e.mkPtr)
    else problem("addressable expression expected for %s", desc)(pos)
  
  
  def addrOf(e: Expr)(pos: Pos): Err[Expr] =
    for(p <- mkPtr(e, "operand")(pos))
    yield p
  
  def deref(e: Expr)(pos: Pos): Err[Expr] = e match {
    case HasType(PointerType(elemT)) => result(PtrDerefLval(e, elemT))
    case _ =>
      problem("operand of pointer indirection/dereference has type %s; pointer type required", e.typeOf)(pos)
  }
  
  @inline
  private def checkIntegral(indx: Expr, desc: String)(pos: Pos): Err[(Expr, IntegralType)] = indx match {
    case UntypedIntegralConst(i)     => result(EvalExpr(PushInt(i, Int32), Int32), Int32)
    case i OfType (iT: IntegralType) => result(i, iT)
    case _ => problem("index type %s is inappropriate for %s; integral type required",
                      indx.typeOf, desc)(pos)
  }
  
  def index(arr: Expr, indx: Expr)(pos: Pos): Err[Expr] = arr match {
    case HasType(ArrayType(_, elemT)) =>
      for ((i, iT) <- checkIntegral(indx, "an array")(pos))
      yield ArrayIndexLval(arr, i, elemT)
    
    case HasType(SliceType(elemT)) =>
      for ((i, iT) <- checkIntegral(indx, "a slice")(pos))
      yield SliceIndexLval(arr, i, elemT)
    
    case HasType(StringType) =>
      for ((i, iT) <- checkIntegral(indx, "a string")(pos))
      yield UnderlyingExpr(arr.evalUnder |+| indx.evalUnder |+| StrIndex(iT), scope.UniverseScope.byte)
    
    case HasType(MapType(keyT, valT)) =>
      for (keyExpr <- convertForAssign(indx, keyT, "specified map key")(pos))
      yield MapIndexLval(arr, keyExpr, valT)
  }
  
  def slice(arr: Expr, low: Option[Expr], high: Option[Expr])(pos: Pos): Err[Expr] = {
    def isIntegralOpt(eOpt: Option[Expr], desc: String) =
      Err.liftOpt(for (e <- eOpt) yield checkIntegral(e, desc)(pos))
    
    for {
      _ <- (isIntegralOpt(low,  "lower bound"),
            isIntegralOpt(high, "upper bound"))
      (boundsStackingCode, boundsInfo) = (low, high) match {
        case (Some(e1), Some(e2)) => (e1.evalUnder |+| e2.evalUnder, BothBounds)
        case (Some(e1), None)     => (e1.evalUnder,                  LowBound)
        case (None,     Some(e2)) => (                 e2.evalUnder, HighBound)
        case (None,     None)     => (CodeBuilder(),                 NoBounds)
      }
      stackingCode = arr.evalUnder |+| boundsStackingCode
      result: Expr <- arr match {
        case HasType(ArrayType(_, t)) => result(EvalExpr(stackingCode |+| SliceArray(t, boundsInfo), SliceType(t)))
        case HasType(SliceType(t))    => result(EvalExpr(stackingCode |+| SliceSlice(t, boundsInfo), SliceType(t)))
        case HasType(StringType)      => result(EvalExpr(stackingCode |+| Substring(boundsInfo),     StringType))
        case _ => problem("cannot slice a value of type %s; array, slice, or string type required", arr.typeOf)(pos)
      }
    } yield result
  }
  
  def incr(e: Expr)(pos: Pos): Err[CodeBuilder] =
    for {
      l <- lval(e, "operand of ++")(pos)
      intT <- integral(l, "operand of ++")(pos) //could tuple together for indep. errors, but why bother?
    } yield l match {
      case VarLval(vr) if vr.typeOf.effective == vr.typeOf.underlying => Incr(vr, 1, intT)
      case _ =>
        val storeEval = UnderlyingExpr(l.loadUnder |+| PushInt(1, intT) |+| Add(intT), l.typeOf).eval
        l.store(storeEval)
    }
  
  def decr(e: Expr)(pos: Pos): Err[CodeBuilder] =
    for {
      l <- lval(e, "operand of --")(pos)
      intT <- integral(l, "operand of --")(pos)
    } yield l match {
      case VarLval(vr) if vr.typeOf.effective == vr.typeOf.underlying => Decr(vr, 1, intT)
      case _ =>
        val storeEval = UnderlyingExpr(l.loadUnder |+| PushInt(1, intT) |+| Sub(intT), l.typeOf).eval
        l.store(storeEval)
    }
  
  
  private def lvalues(es: List[Expr], desc: String)(pos: Pos): Err[List[LvalExpr]] = {
    val lvalsUgly =
      for ((e, i) <- es.zipWithIndex)
      yield for (l <- lval(e, "%s term of %s" format (ordinal(i + 1), desc))(pos))
      yield l
    Err.liftList(lvalsUgly)
  }
  
  private def zipAndCheckArity[A](as: List[A], bs: List[Expr])(pos: Pos): Err[List[(A, Expr)]] = {
    var res: List[(A, Expr)] = Nil
    var (curA, curB) = (as, bs)
    var lenA, lenB = 0
    while (!curA.isEmpty || !curB.isEmpty) {
      if (curA isEmpty) {
        lenB += curB.length
        return problem("arity (%d) of left side unequal to arity (%d) of right side", lenA, lenB)(pos)
      }
      if (curB isEmpty) {
        lenA += curA.length
        return problem("arity (%d) of left side unequal to arity (%d) of right side", lenA, lenB)(pos)
      }
      val pair = (curA.head, curB.head)
      curA = curA.tail
      curB = curB.tail
      res = pair :: res
    }
    result(res reverse)
  }
  
  private def checkAssignAndConvert(e: Expr, t: Type)(pos: Pos): Err[Expr] =
    convertForAssign(e, t, "right side")(pos) //"right side has type %s not assignable..."
  
  private def checkAssignAndConvert(ets: List[(Expr, Type)])(pos: Pos): Err[List[Expr]] = 
    if (ets.length == 1) {
      val (e, t) = ets(0) //ets := plural of (e, t), so to speak. That's where the name comes from.
      checkAssignAndConvert(e, t)(pos) map (_ :: Nil)
    } else {
      val res: Err[List[Expr]] = //conversion!
        for (((e, t), i) <- ets zipWithIndex)
        yield convertForAssign(e, t, "%s expression on right side" format ordinal(i + 1))(pos)
      res
    }
  
  def assign(lefts0: List[Expr], rights0: List[Expr])(pos: Pos): Err[CodeBuilder] =
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
  
  def assign(left0: Expr, right0: Expr)(pos: Pos): Err[CodeBuilder] =
    for ((left, right) <- (lval(left0, "left side of assignment")(pos), checkAssignAndConvert(right0, left0.typeOf)(pos)))
    yield left.store(right.eval)
}
