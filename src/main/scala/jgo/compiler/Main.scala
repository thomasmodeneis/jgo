package jgo.compiler

import lexer._
import parser._
import parser.combinatorExten.ExceptionTracing
import transl._
import interm.symbol.Package

import java.io.{File, FileOutputStream}

object Main extends App {
  val fileName = args(0)
  
  //if fileName == "abc.go", "./abc.go" in current dir.
  //if "/abc.go", in root dir.
  //if "~/abc.go", in home dir.
  val file = new File(fileName)
  
  val scanner = Scanner(file)
  val pkg = Package("package") //add processing of pkg name later
  val comp = new CompilationUnitCompiler(pkg, scanner) with ExceptionTracing
  val intermM = comp.compile
  
  intermM match {
    case Result(interm) =>
      println("intermediate form:\n" + interm)
      val outputBytes = new PkgTranslator(interm).outputBytes
      //he should be closed properly, but we'll just hope the JVM takes care of it
      new FileOutputStream(args(1)).write(outputBytes)
    
    case Problem(errs) =>
      errs foreach { err => println(err.longString); println() }
  }
}
