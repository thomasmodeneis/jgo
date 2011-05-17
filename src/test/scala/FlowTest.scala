import jgo.compiler._
import parser.BlockLang
import parser.combinatorExten._
import lexer.Scanner

import interm.codeseq._

object FlowTest extends ParserTests(
"{ var a, b, c = 1, 2, 3; top: a++; b++; c++; goto top; }",
"{ var i = 0; top: i++; if i < 100 { goto top; }; }",
"""
{
  var i = 1
  var str = "count: "
  goto test
top:
  str = str + "#"
  if i % 4 == 0 {
    str = str + " "
  }
  i++
test:
  if i < 100 {
    goto top
  }
end:
  str = str + "done"
}""",
"""
{
  var i = 1
  var str = "count: "
  for i < 100 {
    str = str + "#"
    if i % 4 == 0 {
      str = str + " "
    }
    i++
  }
  str = str + "done"
}""",
"""
{
  var i = 1
  var str = "count: "
  loop: for i < 100 {
    str = str + "#"
    if i % 4 == 0 {
      str = str + " "
    }
    i++
  }
  str = str + "done"
}""",
"""
{
  for i, str := i, "count: "; i < 100; i++ {
    str = str + "#"
    if i % 4 == 0 {
      str = str + " "
    }
  }
  str = str + "done"
}""",
"""
{
  loop: for i, str := i, "count: "; i < 100; i++ {
    str = str + "#"
    if i % 4 == 0 {
      str = str + " "
    }
  }
  str = str + "done"
}"""
)
