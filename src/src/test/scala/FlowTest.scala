
object FlowTest extends ParserTests(
"{ var a, b, c = 1, 2, 3; top: a++; b++; c++; goto top; }",
"{ var i = 0; top: i++; if i < 100 { goto top; }; }",
"""
{
  var i = 1
  var str = ""
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
  var str = ""
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
  var str = ""
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
  str := ""
  for i := 1; i < 100; i++ {
    str = str + "#"
    if i % 4 == 0 {
      str = str + " "
    }
  }
  str = str + "done"
}""",
"""
{
  str := ""
  loop: for i := 1; i < 100; i++ {
    str = str + "#"
    if i % 4 == 0 {
      str = str + " "
    }
  }
  str = str + "done"
}""",
"""
{
  str := ""
  for i := 1; i < 100; i++ {
    str = str + "#"
    if i % 4 == 0 {
      str = str + " "
    }
    if i > 150 {
      break
    }
  }
  str = str + "done"
}""",
"""
{
  str := ""
  loop: for i := 1; i < 100; i++ {
    str = str + "#"
    if i % 4 == 0 {
      str = str + " "
    }
    if i > 150 {
      break
    }
  }
  str = str + "done"
}""",
"""
{
  str := ""
  loop: for i := 1; i < 100; i++ {
    str = str + "#"
    if i % 4 == 0 {
      str = str + " "
    }
    break loop
  }
  str = str + "done"
}""",
"""
{
  str := ""
  loop: for i := 1; i < 100; i++ {
    str = str + "#"
    if i % 4 == 0 {
      str = str + " "
    }
    if i > 150 {
      break loop
    }
  }
  str = str + "done"
}""",
"""
{
  for {
    for {
      for {
        for { //four!
          break
        }
        break
      }
      break
    }
    break
  }
}""",
"""
{
  for {
    for {
      for {
        for {
          continue
        }
        continue
      }
      continue
    }
    continue
  }
}""",
"""
{
  one: for { for { for { for { break one } } } }
}""",
"""
{
  one: for { for { for { for { continue one } } } }
}"""
)
