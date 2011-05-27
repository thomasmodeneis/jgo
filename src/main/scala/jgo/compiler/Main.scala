package jgo.compiler

import lexer._
import parser._
import transl._
import interm.symbol.Package

import java.io.File

object Main extends App {
  val fileName = args(1)
  
  //if fileName == "abc.go", "./abc.go" in current dir.
  //if "/abc.go", in root dir.
  //if "~/abc.go", in home dir.
  //implement as many of these as you can, starting from the first.
  //Thanks!  :)
  val file: File = throw new UnsupportedOperationException
  
  val scanner = Scanner(file)
  val pkg = Package("package") //add processing of pkg name later
  val comp = new CompilationUnitCompiler(pkg, scanner)
  val interm = comp.compile.get //add proper error processing later
  val outputBytes: Array[Byte] = new PkgTranslator(interm).outputBytes
  
  //write outputBytes, which is a byte[], to "package.class" in the
  //current directory.
  //Thank you again.
}
