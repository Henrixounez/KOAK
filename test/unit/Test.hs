module Test where
import Test.HUnit
import KoakPackrat

simpleExpr = "def test(x : double):double x + 2.0;" -- true
defWrongType = "def test(x : dble):double x + 2.0;" -- false
wrongDefKeyword = "ddzef test(x : double):double x + 2.0;" -- false
unknownVar = "def test(x : double):double x + a;" -- false
testVar = "def test(uneVariable : double):double x + 2.0;" -- true
testIntType = "def test(x : int):double x + 2.0;" -- true
noTypeSeparator = "def test(x double):double x + 2.0;" -- false
testExtern = "extern putchard(x : double): void; def test(x : double):double x + 2.0;" -- true
wrongExternKeyword = "exn putchard(x : double): void; def test(x : double):double x + 2.0;" -- false

testParseFail1 = TestCase (assertEqual "TestFail" "Nothing" (show $ eval defWrongType))
testParseFail2 = TestCase (assertEqual "TestFail" "Nothing" (show $ eval wrongDefKeyword))
testParseFail3 = TestCase (assertEqual "TestFail" "Nothing" (show $ eval unknownVar))
testParseFail4 = TestCase (assertEqual "TestFail" "Nothing" (show $ eval noTypeSeparator))
testParseFail5 = TestCase (assertEqual "TestFail" "Nothing" (show $ eval wrongExternKeyword))

testParseSuccess1 = TestCase (assertNotEqual "TestFail" "Nothing" (show $ eval simpleExpr))
testParseSuccess2 = TestCase (assertNotEqual "TestFail" "Nothing" (show $ eval testVar))
testParseSuccess3 = TestCase (assertNotEqual "TestFail" "Nothing" (show $ eval testIntType))
testParseSuccess4 = TestCase (assertNotEqual "TestFail" "Nothing" (show $ eval testExtern))

tests = TestList [
    TestLabel "testParseFail1" testParseFail1
  , TestLabel "testParseFail2" testParseFail2
  , TestLabel "testParseFail3" testParseFail3
  , TestLabel "testParseFail4" testParseFail4
  , TestLabel "testParseFail5" testParseFail5
  , TestLabel "testParseExpr1" testParseSuccess1
  , TestLabel "testParseExpr2" testParseSuccess2
  , TestLabel "testParseExpr3" testParseSuccess3
  , TestLabel "testParseExpr4" testParseSuccess4
  ]
