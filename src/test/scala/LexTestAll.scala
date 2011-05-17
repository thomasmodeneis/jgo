import jgo.compiler._
import parser.BlockLang
import parser.combinatorExten._
import lexer._

import interm.codeseq._

import java.io.{File, InputStream, FileInputStream, InputStreamReader}

object LexTestAll {
  def main(args: Array[String]) {
    if (args.isEmpty)
      testAll(new File(System.getProperty("user.home") + "/Desktop/gotest/"))
    else
      testAll(new File(args(0)))
  }
  
  def testAll(dir: File) {
    for (file <- dir.listFiles)
      if (file.isDirectory)
        testAll(file)
      else if (file.isFile && !file.isHidden)
        test(file)
  }
  
  def test(file: File) {
    println("testing: " + file.getCanonicalPath)
    println()
    
    var cur = Scanner(file)
    print("tokenization: ")
    while (!cur.atEnd) {
      print(cur.first + " ")
      cur = cur.rest
    }
    println()
    println()
  }
}
