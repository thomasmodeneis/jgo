import jgo.compiler._
import parser.BlockLang
import parser.combinatorExten._
import lexer.Scanner

import message._
import interm.codeseq._

object LexTest {
  def main(args: Array[String]) {
    test("{ }")
    test("{ ; }")
    test("{ { } }")
    test("{ { }; { } }")
    test("{ var x int }")
    test("""
{
  "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "                                          "
  "abcdefghijklmnopqrstuvwxyz\n0123456789\nABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "\nabcdefghijklmnopqrstuvwxyz\n0123456789\nABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "\nabcdefghijklmnopqrstuvwxyz\n0123456789\nABCDEFGHIJKLMNOPQRSTUVWXYZ\n"
  "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
}""")
    test("""
{
  var x, y int
  var s1, s2 string
  var a [10]int
  x, y = y, x
  s1 = s1 + s2
  x, y = x / y, x % y
  var u uint
  x << u
  x >> u
}""")
    test("""
{
  var x, y int //literals not yet supported in the grammar
  var s string
  if x < y {
    s = s + s
  }
  x++
  y--
}""")
    test("""
{
  var x, y int //literals not yet supported in the grammar
  var s string
  if x < y {
    s = s + s
  } else {
    s = s + s + s
  }
  x++
  y--
}""")
    test("""
{
  var x, y int //literals not yet supported in the grammar
  var s string
  y++
  y++
  y++
  for x < y {
    s = s + s
    x++
  }
  x++
  y--
}""")
    test("""
{
  var x, y int //literals not yet supported in the grammar
  var s string
  y++
  y++
  y++
  for x < y {
    if x + x > y {
      s1 := s + s
      s := s1
    } else {
      s = s + s + s
    }
    x++
  }
  x++
  y--
}""")
    
    test("""
{
  var x, y int //literals not yet supported in the grammar
  b0 := x < y
  
  b1 := x < y && x < y
  b2 := x < y || x < y
  
  ba := x < y && x < y && x < y
  bb := x < y || x < y || x < y
  
  b3 := (x < y && x < y) || x < y
  b4 := (x < y || x < y) && x < y
  
  b5 := x < y && (x < y || x < y)
  b6 := x < y || (x < y && x < y)
  
  b7 := (x < y || x < y) && (x < y || x < y)
  b8 := (x < y && x < y) || (x < y && x < y)
  
  b := x < y && x > y || x == y || x != y && !(x == y)
  
  b = !b
  b = !!b
  b = !!!b
  b = !!!!b
  b = !!!!!b
}""")
    test("""
{
  var x, y int //literals not yet supported in the grammar
  b0 := x < y
}""")
    test("""
{
  var x, y int //literals not yet supported in the grammar
  b0 := x < y
  b1 := x > y
}""")
    test("""
{
  var x, y int //literals not yet supported in the grammar
  b0 := x < y
  b1 := x > y
  
  var b2 bool
  b2 = b0 && b1
  b2 = b0 || b1
  
  var b3 bool
  b3 = !b0 && b1
  b3 = !b0 || b1
  
  var b4 bool
  b4 = b0 && !b1
  b4 = b0 || !b1
  
  /*
  var b5 bool
  b5 = b0 && b1 && b2
  b5 = b0 || b1 || b2
  
  var b6 bool
  b6 = (b0 && b1) || b2
  b6 = (b0 || b1) && b2
  */
  
  bNand := !(b0 && b1)
  bNor  := !(b0 || b1)
  
  bNor_, bNand_ := !b0 && !b1, !b0 || !b1
  
  error := false
  if (bNand != bNand_) {
    error = true
  }
  if (bNor == bNor_) {
    //do nothing
  } else {
    error = true
  }
  
  error2 := false
  if (true == (bNand != bNand_)) {
    error2 = true
  }
}""")
    test("""
{
  b := true
  
  var zero int
  b = b
  
  var one int
  b = !b
  
  var two int
  b = !!b
  
  var three int
  b = !!!b
  
  var four int
  b = !!!!b
}""")
    
    test("""
{
  var x, y int //literals not yet supported in the grammar
  var z, w int64
  var u uint
  var f float64
  var s string
  var c chan int
  var sl []int
  var ar [10]int
  f++ //can't ++ a float
  x + z //can't add values of differing types
  x << y //can't shift by signed value
  s[x:y]  
  sl[x:y] 
  ar[x:y] 
  s[x]    
  sl[x]   
  ar[x]   
  x = <-c
  x
  c <- y
  x <- y //x is not a chan
  <-x    //not a chan
  var (
    cSend  chan<- int  = c
    cRecv  <-chan int  = c
    cSendC chan<- chan int
    cc     chan chan int
  )
  <-<-cc
  cc <- <-cc
  cSendC <- c
  c = <-cSendC //bad
  cSend <- x
  x = <-cSend //bad
  cRecv <- x //bad
  x = <-cRecv
}""")
    test("""
{
  x, y, z := 1, 2, 3
  
  hello := 5 / 3 + 7 * 5 - 1
  
  w := -(-(-(-(x))))
  
  a := x + 5
  b := x - 6
  c := x * 7
  d := x / 8
  e := x * -9
  f := x - -10
  g := 5 + x
  h := 6 - x
  i := 7 * x
  j := 8 / x
  k := -9 * x
  l := -10 / x
  
  enough := "STOP!!!"
  zero, one := 4 % 2, 3 % 2
  reply := "OK.  Done.  Happy now?\n"
}""")
    test("""
{
  a := 1
  b := 2
  {
    c, d := 3, 4
    {
      e := 5
    }
    {
      f := "hello"
      {
        g := "goodbye"
      }
      h := "Harrison"
      {
        i := "I never get tired of scoping!"
      }
      {
        {
          {
            {
              j := "Eh. Ok, perhaps I do."
            }
          }
        }
      }
    }
  }
}""")
    test("""
{
  a := 1
  b := 2
  a++
  {
    c, d := 3, 4
    c++
    a++
    {
      e := 5
      c++
      e++
      a++
    }
    {
      f := "hello"
      f = f + "!"
      c++
      a++
      {
        g := "goodbye"
        g = g + "!"
        f = f + "!"
        c++
        a++
      }
      h := "Harrison"
      h = h + "!"
      f = f + "!"
      c++
      a++
      {
        i := "I never get tired of scoping"
        i = i + "!"
        h = h + "!"
        f = f + "!"
        c++
        a++
      }
      h = h + "!"
      f = f + "!"
      c++
      a++
      {
        a++
        {
          a++
          {
            a++
            {
              a++
              j := "Eh. Ok, perhaps I do"
              j = j + "!"
              a++
            }
          }
        }
      }
      b++ //WOAH!  Unprecedented!
    }
  }
}""")
  }
  
  def test(in: String) {
    println("testing: " + in)
    println()
    
    var cur = Scanner(in)
    print("tokenization: ")
    while (!cur.atEnd) {
      print(cur.first + " ")
      cur = cur.rest
    }
    println()
    println()
  }
}
