module Main where
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

testParseFail1 = TestCase (assertEqual "TestFail" "Left (Just \"syntax error: error on 'def' definition\")" (show $ eval defWrongType))
testParseFail2 = TestCase (assertEqual "TestFail" "Left (Just \"syntax error: Error on 'extern' definition\")" (show $ eval wrongDefKeyword))
testParseFail3 = TestCase (assertEqual "TestFail" "Left (Just \"syntax error: error on 'def' definition\")" (show $ eval noTypeSeparator))
testParseFail4 = TestCase (assertEqual "TestFail" "Left (Just \"syntax error: Error on 'extern' definition\")" (show $ eval wrongExternKeyword))

testParseSuccess1 = TestCase (assertEqual "TestFail" "Right file:\n[def:\n\"test\":\n\targs: [(\"x\",double)]\n\ttype: double\ninstructions:\n[+: (2.0, \"x\")]\n\n]" (show $ eval simpleExpr))
testParseSuccess2 = TestCase (assertEqual "TestFail" "Right file:\n[def:\n\"test\":\n\targs: [(\"uneVariable\",double)]\n\ttype: double\ninstructions:\n[+: (2.0, \"x\")]\n\n]" (show $ eval testVar))
testParseSuccess3 = TestCase (assertEqual "TestFail" "Right file:\n[def:\n\"test\":\n\targs: [(\"x\",int)]\n\ttype: double\ninstructions:\n[+: (2.0, \"x\")]\n\n]" (show $ eval testIntType))
testParseSuccess4 = TestCase (assertEqual "TestFail" "Right file:\n[extern: \"putchard\"\n\targs: [(\"x\",double)]\n\ttype: void,def:\n\"test\":\n\targs: [(\"x\",double)]\n\ttype: double\ninstructions:\n[+: (2.0, \"x\")]\n\n]" (show $ eval testExtern))

tests = TestList [
    TestLabel "testParseFail1" testParseFail1
  , TestLabel "testParseFail2" testParseFail2
  , TestLabel "testParseFail3" testParseFail3
  , TestLabel "testParseFail4" testParseFail4
  , TestLabel "testParseExpr1" testParseSuccess1
  , TestLabel "testParseExpr2" testParseSuccess2
  , TestLabel "testParseExpr3" testParseSuccess3
  , TestLabel "testParseExpr4" testParseSuccess4
  ]

main = runTestTT tests