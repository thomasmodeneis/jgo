package jgo.compiler
package parser

import interm._
import types._
import message._

trait TypeSyntax extends Symbols {
  lazy val goType: P[Type] =                                                                "type" $
    ( typeName
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
  
  
  lazy val typeName: P[TypeName] =                                                     "type-name" $
    //qualifiedIdent
    typeSymbol  ^^ { _.underlying }
  
  
  lazy val arrayType: P[ArrayType] =                                                  "array type" $
    //("[" ~> expression <~ "]") ~ goType //compile-time constants not yet fully supported
    ("[" ~> intLit <~ "]") ~ goType  ^^ {
      case i ~ t => 
        val len = i.value.asInstanceOf[Int]
        errIf(len < 0, "cannot have negative array length")
        ArrayType(len, t)
    }
  
  
  lazy val sliceType: P[SliceType] =                                                  "slice type" $
    "[" ~ "]" ~> goType  ^^ { SliceType(_) }
  
  
  lazy val structType: P[StructType] =                                               "struct type" $
    "struct" ~>! "{" ~> repWithSemi(structFieldDecl) <~ "}"  ^^ {
      val fields = _.flatten
      StructType(fields)
      //add logic for duplicate field names, etc
    }
  
  lazy val structFieldDecl: P[List[FieldDesc]] =                               "struct field decl" $
    ( identList ~ goType ~ stringLit.?  ^^ { case ids ~ t ~ tag => ids map { RegularFieldDesc(_, t, tag) } }
    | "*" ~> typeName    ~ stringLit.?  ^^ { case t ~ tag       => EmbeddedFieldDesc(t.name, t, true,  tag) }
    |        typeName    ~ stringLit.?  ^^ { case t ~ tag       => EmbeddedFieldDesc(t.name, t, false, tag) }
    )
  
  
  lazy val pointerType: P[PointerType] =                                            "pointer type" $
    "*" ~> goType  ^^ { PointerType(_) }
  
  
  lazy val functionType: P[FuncType] =                                             "function type" $
    "func" ~>! funcTypeParams ~ funcTypeResult.?  ^^ {
      case (params, isVariadic) ~ Some(results) => FuncType(params, resuts, isVariadic)
      case (params, isVariadic) ~ None          => FuncType(params,    Nil, isVariadic)
    } 
  
  lazy val funcTypeResult: P[List[Type]] =                                  "function-type result" $
    ( goType         ^^ { List(_) }
    | funcTypeParams ^^ {
        case (results, isVariadic) =>
          withErrIf(isVariadic, "Result \"parameter\" list may not be variadic") {
            results
          }
      }
    )
  
  lazy val funcTypeParams: P[(List[Type], Boolean)] =                   "function-type parameters" $
    "(" ~> repsep(funcParamDecl, ",") <~ ")"  ^^ { //recall that repsep admits zero repetitions
      val (backwardsParams, variadic, err) = (_ foldLeft ((Nil, false, false))) {
        case ((lsAcc, isPrevVariadic, hasErr), (ls, isVariadic)) =>
          //if the previous decl was variadic (isVariadicAcc), set hasErr to true.
          (ls reverse_::: lsAcc, isVariadic, isPrevVariadic || hasErr)
      }
      withErrIf(err, "`...' permitted only on the final type in a signature") {
        (backwardsParams.reverse, variadic)
      }
    }
  
  //This needs to be improved, and made more succinct.  Also, account for the fact that
  //"either all of the parameter names are present in a param list, or all are absent"
  lazy val funcParamDecl: P[(List[Type], Boolean)] =           "function parameter(s) declaration" $
    ((identList <~ "...") ~ goType  ^^ { case is ~ t => (List.fill(is.length)(t), true) }
    | identList ~ goType            ^^ { case is ~ t => (List.fill(is.length)(t), false) }
    | "..." ~> goType               ^^ { (List(_), true) }
    | goType                        ^^ { (List(_), false) }
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
    "map" ~>! ("[" ~> goType <~ "]") ~ goType  ^^ { //the first type is that of the keys; the second, the vals
      case k ~ v => MapType(k, v)
    }
  
  //figure out how to resolve the ambiguity here such that
  //"[t]he <- operator associates with the leftmost chan possible."
  //I think this does it!
  lazy val channelType: P[ChanType] =                                               "channel type" $
    ( "chan" ~> "<-" ~> goType  ^^ { ChanType(_, canRecieve = false, canSend = true) }
    | "<-" ~> "chan" ~> goType  ^^ { ChanType(_, canRecieve = true,  canSend = false) }
    | "chan" ~> goType          ^^ { ChanType(_, canRecieve = true,  canSend = true) }
    )
  
  
  lazy val typeNoAmbigPrefix: P_ =                              "type without an ambiguous prefix" $
    not("<-").named("not `<-'") ~> goType
  
  
  lazy val typeNoAmbigPrefixNoIdent: P_ =     "type without an ambiguous prefix or prefixal ident" $
    ( not("<-").named("not `<-'")
    ~ not(rep("(") ~ ident).named("forbid idents")
    ~> goType
    )
  
  
  lazy val typeList: P[List[Type]] =                                                   "type list" $
    rep1sep(goType, ",")
}
