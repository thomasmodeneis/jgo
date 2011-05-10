package jgo.compiler
package parser.types

import message._

import parser.scoped._

import interm.types._
import interm.symbol._

trait TypeSyntax extends Symbols with TypeUtils {
  lazy val goType: PM[Type] =                                                                "type" $
    ( typeSymbol ^^ { _ map { symb => symb.theType } }
    | "(" ~> goType <~ ")"   &@ "parenthesized type"
    | arrayType
    | structType
    | pointerType
    | functionType
  //| interfaceType //not currently supported in the type system
    | sliceType
    | mapType
    | channelType
    )
  
  
  lazy val arrayType: PM[ArrayType] =                                                  "array type" $
    //("[" ~> expression <~ "]") ~ goType //compile-time constants not yet fully supported
    "[" ~> intLit ~ pos("]") ~ goType  ^^ array
  
  
  lazy val sliceType: PM[SliceType] =                                                  "slice type" $
    "[" ~ "]" ~> goType  ^^ { _ map SliceType.apply }
  
  
  lazy val structType: PM[StructType] =                                               "struct type" $
    "struct" ~>! "{" ~> repWithSemi(structFieldDecl) <~ "}"  ^^ struct
  
  private lazy val structFieldDecl: PM[List[FieldDesc]] =                       "struct field decl" $
    ( identList ~ goType ~ stringLit.?  ^^ regularFieldDecl
    | "*" ~> typeSymbol  ~ stringLit.?  ^^ embeddedFieldDecl(true)
    |        typeSymbol  ~ stringLit.?  ^^ embeddedFieldDecl(false)
    )
  
  
  lazy val pointerType: PM[PointerType] =                                            "pointer type" $
    "*" ~> goType  ^^ { _ map PointerType.apply }
  
  
  lazy val functionType: PM[FuncType] =                                             "function type" $
    "func" ~>! params ~ results.?  ^^ func
  
  private lazy val params: PM[(List[Type], Boolean)] =                   "function-type parameters" $
    "(" ~> repsep(paramGroup, ",") <~ ")"  ^^ funcParams
  
  //This needs to be improved, and made more succinct.  Also, account for the fact that
  //"either all of the parameter names are present in a param list, or all are absent"
  private lazy val paramGroup: PM[(List[Type], Option[Pos])] =      "function-type parameter group" $
    ( identList ~ pos("...").? ~ goType  ^^ identParamGroup
    | pos("...").? ~ goType              ^^ typeParamGroup
    )
  
  private lazy val results: PM[List[Type]] =                                 "function-type result" $
    ( goType                                  ^^ enlist
    | "(" ~> repsep(resultGroup, ",") <~ ")"  ^^ flatten
    )
  
  private lazy val resultGroup: PM[List[Type]] =                       "function-type result group" $
    ( identList ~ goType  ^^ countAndFill
    | goType              ^^ enlist
    )
  
  /*
  lazy val interfaceType: P_ =                        "interface type" $
    "interface" ~>! "{" ~> repWithSemi(interfMethodSpec) <~ "}"
  
  lazy val interfMethodSpec: P_ =         "interface method specifier" $
    ( ident ~ funcSignature
    | typeName              //the name of another interface, whose method-set is to be included
    )
  */
  
  lazy val mapType: PM[MapType] =                                                        "map type" $
    "map" ~>! ("[" ~> goType <~ "]") ~ goType  ^^ mkMap
  
  lazy val channelType: PM[ChanType] =                                               "channel type" $
    ( "chan" ~> "<-" ~> goType  ^^ chan(recv = false, send = true)
    | "<-" ~> "chan" ~> goType  ^^ chan(recv = true,  send = false)
    | "chan" ~> goType          ^^ chan(recv = true,  send = true)
    )
  
  lazy val typeList: PM[List[Type]] =                                                   "type list" $
    rep1sep(goType, ",")
  
  
  
  private def enlist[T](tM: M[T]) =
    for (t <- tM)
    yield t :: Nil
  
  private def flatten[T](lsUgly: List[M[List[T]]]) = {
    val lsM: M[List[List[T]]] = lsUgly
    for (ls <- lsM)
    yield ls.flatten
  }
  
  private def countAndFill(is: List[String], tM: M[Type]) =
    for (t <- tM)
    yield for (i <- is)
    yield t
  
  
  
  private def array(i: lexer.IntLit, pos: Pos, tM: M[Type]) = tM flatMap { t =>
    val len = i.value.toInt
    if (len < 0) Problem("cannot have negative array length")(pos)
    else Result(ArrayType(len, t))
  }
  
  private def mkMap(kM: M[Type], vM: M[Type]) =
    for ((k, v) <- together2(kM, vM))
    yield MapType(k, v)
  
  private def chan(recv: Boolean, send: Boolean)(elemTM: M[Type]) = elemTM map { elemT =>
    ChanType(elemT, canRecv = recv, canSend = send)
  }
  
  
  
  private def struct(declsUgly: List[M[List[FieldDesc]]]) = {
    val declsM: M[List[List[FieldDesc]]] = declsUgly
    for {
      decls <- declsM
      fields = decls.flatten
    } yield StructType(fields)
    //add logic for duplicate field names, etc
  }
  
  private def regularFieldDecl(ids: List[String], tM: M[Type], tag: Option[String]) =
    for (t <- tM)
    yield ids map { id => RegularFieldDesc(id, t, tag) }
  
  private def embeddedFieldDecl(isPtr: Boolean)(tM: M[TypeSymbol], tag: Option[String]) =
    for (tSymb <- tM; t: NamedType = tSymb)
    yield List(EmbeddedFieldDesc(t.name, t, isPtr,  tag))
  
  
  
  private def func(paramInfoM: M[(List[Type], Boolean)], resultInfoUgly: Option[M[List[Type]]]) = {
    val resultInfoM: M[Option[List[Type]]] = resultInfoUgly
    for (((params, isVariadic), resultInfo) <- together2(paramInfoM, resultInfoM))
    yield resultInfo match {
      case Some(results) => FuncType(params, results, isVariadic)
      case None          => FuncType(params,     Nil, isVariadic)
    }
  }
  
  private def funcParams(lsUgly: List[M[(List[Type], Option[Pos])]]): M[(List[Type], Boolean)] = {
    val lsM: M[List[(List[Type], Option[Pos])]] = lsUgly
    
    lsM flatMap { ls =>
      val base: M[(List[Type], Boolean)] = Result(Nil: List[Type], false)
      (ls foldRight base) {
        (next, accM) =>
          val (nextGroup, ellipsisPosOpt) = next
          accM flatMap {
            case (Nil, _) => Result(nextGroup, ellipsisPosOpt.isDefined)
            case (paramsOnRight, isVariadic) =>
              ellipsisPosOpt match {
                case Some(pos) => Problem("... permitted only on the final type in a signature")(pos)
                case None      => Result(nextGroup ::: paramsOnRight, isVariadic)
              }
          }
      }
    }
  }
  
  private def identParamGroup(is: List[String], posOpt: Option[Pos], tM: M[Type]) =
    for (t <- tM)
    yield (List.fill(is.length)(t), posOpt)
  
  private def typeParamGroup(posOpt: Option[Pos], tM: M[Type]) =
    for (t <- tM)
    yield (t :: Nil, posOpt)
}
