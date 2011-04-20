package jgo.compiler
package parser

import interm._
import types._
import symbols._

trait TypeSyntax extends Symbols with TypeUtils {
  lazy val goType: P[Type] =                                                                "type" $
    ( typeSymbol
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
  
  
  lazy val typeSymbol: P[Type with Named] =                                            "type-name" $
    ident ^^ typeSymb
    //qualifiedIdent
    //typeSymbol  ^^ { _.typeName }
  
  
  lazy val arrayType: P[ArrayType] =                                                  "array type" $
    //("[" ~> expression <~ "]") ~ goType //compile-time constants not yet fully supported
    ("[" ~> intLit <~ "]") ~ goType  ^^ array
  
  
  lazy val sliceType: P[SliceType] =                                                  "slice type" $
    "[" ~ "]" ~> goType  ^^ SliceType
  
  
  lazy val structType: P[StructType] =                                               "struct type" $
    "struct" ~>! "{" ~> repWithSemi(structFieldDecl) <~ "}"  ^^ struct
  
  lazy val structFieldDecl: P[List[FieldDesc]] =                               "struct field decl" $
    ( identList ~ goType ~ stringLit.?  ^^ regularFieldDecl
    | "*" ~> typeSymbol  ~ stringLit.?  ^^ embeddedFieldDecl(true)
    |        typeSymbol  ~ stringLit.?  ^^ embeddedFieldDecl(false)
    )
  
  
  lazy val pointerType: P[PointerType] =                                            "pointer type" $
    "*" ~> goType  ^^ PointerType
  
  
  lazy val functionType: P[FuncType] =                                             "function type" $
    "func" ~>! funcTypeParams ~ funcTypeResult.?  ^^ func
  
  lazy val funcTypeParams: P[(List[Type], Boolean)] =                   "function-type parameters" $
    "(" ~> repsep(funcTypeParamDecl, ",") <~ ")"  ^^ funcParams
  
  //This needs to be improved, and made more succinct.  Also, account for the fact that
  //"either all of the parameter names are present in a param list, or all are absent"
  lazy val funcTypeParamDecl: P[(List[Type], Boolean)] =       "function parameter(s) declaration" $
    ((identList <~ "...") ~ goType  ^^ funcParamIdentDecl(true)
    | identList ~ goType            ^^ funcParamIdentDecl(false)
    | "..." ~> goType               ^^ funcParamTypeDecl(true)
    | goType                        ^^ funcParamTypeDecl(false)
    )
  
  lazy val funcTypeResult: P[List[Type]] =                                  "function-type result" $
    ( goType                                         ^^ enlist
    | "(" ~> repsep(funcTypeResultDecl, ",") <~ ")"  ^^ flatten
    )
  
  lazy val funcTypeResultDecl: P[List[Type]] =
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
  
  lazy val mapType: P[MapType] =                                                        "map type" $
    "map" ~>! ("[" ~> goType <~ "]") ~ goType  ^^ MapType
  
  lazy val channelType: P[ChanType] =                                               "channel type" $
    ( "chan" ~> "<-" ~> goType  ^^ chan(recv = false, send = true)
    | "<-" ~> "chan" ~> goType  ^^ chan(recv = true,  send = false)
    | "chan" ~> goType          ^^ chan(recv = true,  send = true)
    )
  
  lazy val typeList: P[List[Type]] =                                                   "type list" $
    rep1sep(goType, ",")
  
  
  private def enlist[T](t: T):               List[T] = t :: Nil
  private def flatten[T](ls: List[List[T]]): List[T] = ls.flatten
  
  private def countAndFill(is: List[String], t: Type):  List[Type] =
    for (i <- is) yield t
  
  private val typeSymb: String => Type with Named = getTypeSymbol(_) match {
    case Some(t: TypeSymbol) => t.theType
    case _                   => TypeError
  }
  
  private def array(i: lexical.IntLit, t: Type) = {
    val len = i.value.asInstanceOf[Int]
    errIf(len < 0, "cannot have negative array length")
    ArrayType(len, t)
  }
  
  private def chan(recv: Boolean, send: Boolean)(elemT: Type) =
    ChanType(elemT, canReceive = recv, canSend = send)
  
  
  private def struct(decls: List[List[FieldDesc]]) = {
    val fields = decls.flatten
    StructType(fields)
    //add logic for duplicate field names, etc
  }
  
  private def regularFieldDecl(ids: List[String], t: Type, tag: Option[String]) =
    ids map { id => RegularFieldDesc(id, t, tag) }
  
  private def embeddedFieldDecl(isPtr: Boolean)(t: Type with Named, tag: Option[String]) =
    List(EmbeddedFieldDesc(t.name, t, isPtr,  tag))
  
  
  private def func(paramInfo: (List[Type], Boolean), resultInfo: Option[List[Type]]): FuncType = {
    val (params, isVariadic) = paramInfo
    resultInfo match {
      case Some(results) => FuncType(params, results, isVariadic)
      case None          => FuncType(params,     Nil, isVariadic)
    }
  }
  
  private def funcParams(ls: List[(List[Type], Boolean)]): (List[Type], Boolean) = {
    val (decls, variadics) = ls.unzip
    val (err, variadic) = (variadics.init contains true, variadics last)
    errIf(err, "`...' permitted only on the final type in a signature")
    (decls.flatten, variadic)
  }
  
  private def funcParamIdentDecl(variadic: Boolean)(is: List[String], t: Type) =
    (List.fill(is.length)(t), variadic)
  
  private def funcParamTypeDecl(variadic: Boolean)(t: Type) =
    (t :: Nil, variadic)
  
  
  
}
