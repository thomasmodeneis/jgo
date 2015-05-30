package jgo.tools.compiler
package parser.types

import parser.scoped._
import parser.funcs.Signatures
import parser.exprs.Expressions

import interm._
import types._
import expr._
import symbol._

trait Types extends Symbols with Signatures {
  self: Expressions =>
  
  lazy val goType: Rule[Type] =                                                             "type" $
    ( typeSymbol ^^ { _ map (_.theType) }
    | "(" ~> goType <~ ")"
    | arrayType
    | structType
    | pointerType
    | funcType
  //| interfaceType //not currently supported in the type system
    | sliceType
    | mapType
    | channelType
    )
  
  /**
   * A parser that admits only types and backtracks on all other input.
   * Used for type-calls (T must be a type in new(T)) and conversions
   * (T(x) is a conversion if T is a type, function call otherwise).
   */
  lazy val onlyGoType: Rule[Type] =                                                    "only-type" $
    ( onlyTypeSymbol  ^^ (_.theType) ^^ result
    | "(" ~> onlyGoType <~ ")"
    | arrayType
    | structType
    | pointerType
    | funcType
  //| interfaceType //not currently supported in the type system
    | sliceType
    | mapType
    | channelType
    )
  
  
  lazy val arrayType: Rule[ArrayType] =                                               "array type" $
    "[" ~> expression ~ "]" ~ goType  ^^ mkArray
  
  
  lazy val sliceType: Rule[SliceType] =                                               "slice type" $
    "[" ~ "]" ~> goType  ^^ { _ map SliceType.apply }
  
  
  lazy val structType: Rule[StructType] =                                            "struct type" $
    "struct" ~>! "{" ~> repWithSemi(structFieldDecl) <~ "}"  ^^ mkStruct
  
  private lazy val structFieldDecl: Rule[List[FieldDesc]] =                    "struct field decl" $
    ( identList ~ goType ~ stringLit.?  ^^ regularFieldDecl
    | "*" ~> typeSymbol  ~ stringLit.?  ^^ embeddedFieldDecl(true)
    |        typeSymbol  ~ stringLit.?  ^^ embeddedFieldDecl(false)
    )
  
  
  lazy val pointerType: Rule[PointerType] =                                         "pointer type" $
    "*" ~> goType  ^^ { _ map PointerType.apply }
  
  
  lazy val funcType: Rule[FuncType] =                                                  "func type" $
    "func" ~>! signature  ^^ { _ map (_.typeOf) }
  
  
  /*
  lazy val interfaceType: P_ =                        "interface type" $
    "interface" ~>! "{" ~> repWithSemi(interfMethodSpec) <~ "}"
  
  lazy val interfMethodSpec: P_ =         "interface method specifier" $
    ( ident ~ funcSignature
    | typeName              //the name of another interface, whose method-set is to be included
    )
  */
  
  
  lazy val mapType: Rule[MapType] =                                                     "map type" $
    "map" ~>! ("[" ~> goType <~ "]") ~ goType  ^^ mkMap
  
  
  lazy val channelType: Rule[ChanType] =                                            "channel type" $
    ( "chan" ~> "<-" ~> goType  ^^ mkChan(recv = false, send = true)
    | "<-" ~> "chan" ~> goType  ^^ mkChan(recv = true,  send = false)
    | "chan" ~> goType          ^^ mkChan(recv = true,  send = true)
    )
  
  
  lazy val typeList: Rule[List[Type]] =                                                "type list" $
    rep1sep(goType, ",")
  
  
  
  private def mkArray(eErr: Err[Expr], pos: Pos, tErr: Err[Type]) =
    (eErr, tErr) flatMap { case (e, t) =>
      e match {
        case IntegralConst(len) =>
          if (!len.isValidInt)
            problem("array length must be representable as an int")(pos)
          else if (len < 0)
            problem("array length must be non-negative")(pos)
          else
            result(ArrayType(len.toInt, t))
        case _ => problem("array length must be an integral constant")(pos)
      }
    }
  
  private def mkMap(kErr: Err[Type], vErr: Err[Type]) =
    for ((k, v) <- (kErr, vErr))
    yield MapType(k, v)
  
  private def mkChan(recv: Boolean, send: Boolean)(elemTErr: Err[Type]) = elemTErr map { elemT =>
    ChanType(elemT, canRecv = recv, canSend = send)
  }
  
  
  private def mkStruct(declsUgly: List[Err[List[FieldDesc]]]) = {
    val declsErr: Err[List[List[FieldDesc]]] = declsUgly
    for {
      decls <- declsErr
      fields = decls.flatten
    } yield StructType(fields)
    //add logic for duplicate field names, etc
  }
  
  private def regularFieldDecl(ids: List[String], tErr: Err[Type], tag: Option[String]) =
    for (t <- tErr)
    yield ids map { id => RegularFieldDesc(id, t, tag) }
  
  private def embeddedFieldDecl(isPtr: Boolean)(tErr: Err[TypeSymbol], tag: Option[String]) =
    for (tSymb <- tErr; t: NamedType = tSymb)
    yield List(EmbeddedFieldDesc(t.name, t, isPtr,  tag))
}
