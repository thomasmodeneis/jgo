package jgo.compiler
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
    ( typeSymbol ^^ { _ map { symb => symb.theType } }
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
  
  //TODO:  This shouldn't be necessary.  Or should it?  In which case, document.
  //Used by FunctionCompiler and its kin.
  lazy val onlyGoType: Rule[Type] =                                                    "only-type" $
    ( onlyTypeSymbol  ^^ { _.theType } ^^ M
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
    "func" ~>! signature  ^^ { _ map { _.typeOf } }
  
  
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
  
  
  
  private def mkArray(eM: M[Expr], pos: Pos, tM: M[Type]) =
    (eM, tM) flatMap { case (e, t) =>
      e match {
        case IntegralConst(len) =>
          if (!len.isValidInt) Problem("array length must be representable as an int")(pos)
          else if (len < 0) Problem("array length must be non-negative")(pos)
          else Result(ArrayType(len.toInt, t))
        case _ => Problem("array length must be an integral constant")(pos)
      }
    }
  
  private def mkMap(kM: M[Type], vM: M[Type]) =
    for ((k, v) <- (kM, vM)) yield
      MapType(k, v)
  
  private def mkChan(recv: Boolean, send: Boolean)(elemTM: M[Type]) = elemTM map { elemT =>
    ChanType(elemT, canRecv = recv, canSend = send)
  }
  
  
  private def mkStruct(declsUgly: List[M[List[FieldDesc]]]) = {
    val declsM: M[List[List[FieldDesc]]] = declsUgly
    for {
      decls <- declsM
      fields = decls.flatten
    } yield StructType(fields)
    //add logic for duplicate field names, etc
  }
  
  private def regularFieldDecl(ids: List[String], tM: M[Type], tag: Option[String]) =
    for (t <- tM) yield
      ids map { id => RegularFieldDesc(id, t, tag) }
  
  private def embeddedFieldDecl(isPtr: Boolean)(tM: M[TypeSymbol], tag: Option[String]) =
    for (tSymb <- tM; t: NamedType = tSymb) yield
      List(EmbeddedFieldDesc(t.name, t, isPtr,  tag))
}
