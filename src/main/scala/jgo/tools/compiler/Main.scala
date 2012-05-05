package jgo.tools.compiler

import lexer._
import parser._
import parser.combinatorExten._
import transl._
import interm.symbol.PackageSymbol

import java.io.{File, FileOutputStream}

object Main extends App {
  val fileName = args(0)
  
  //if fileName == "abc.go", "./abc.go" in current dir.
  //if "/abc.go", in root dir.
  //if "~/abc.go", in home dir.
  val file = new File(fileName)
  
  val scanner = Scanner(file)
  val pkg = PackageSymbol("package") //add processing of pkg name later
  val comp = new CompilationUnitCompiler(pkg, scanner) with ExceptionTracing
  val intermErr = comp.compile
  
  intermErr match {
    case Result(interm) =>
      val outputBytes = new PkgTranslator(interm).outputBytes
      //he should be closed properly, but we'll just hope the JVM takes care of it
      new FileOutputStream(args(1)).write(outputBytes)
      
      //Let the record show that this is the single piece of code my friend Aaron has
      //committed to the project, and not only does Aaron fail to close his file;
      //he also anthropomorphizes it! I'd fix this now, but it's not really worth
      //it given that I'm going to have to replace this whole thing anyway with
      //something that does multiple files. In the meantime, may the gods of IO,
      //low latency be upon them, have mercy on my neural configuration.
    
    case Problems(errs) =>
      errs foreach { err => println(err.longString); println() }
  }
}
