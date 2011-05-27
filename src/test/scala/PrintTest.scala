
object PrintTest extends ParserTests(
  "{ }",
  "{ print 3 }",
  "{ print 3; }",
  "{ print \"hello, world\" }",
  "{ print \"hello, world\"; }"
)
