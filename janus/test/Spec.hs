{-# LANGUAGE TemplateHaskell #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.List ((\\))
import Language.Haskell.TH
import System.IO.Unsafe (unsafePerformIO)

import AST
import Parser.JanusParser
import SemanticChecker (extractVarsE, semanticCheck)

import Debug.Trace

main = defaultMain
  [ constructTestSuite testName testSuite
  | (testName, testSuite) <- [ ("PRS", parserTests)
                             , ("VAR", varTests)
                             , ("SEM", semanticTests)
                             ]
  ]

constructTestSuite s suite =
  testGroup s [testCase (s ++ "_" ++ show i) t | (i, t) <- zip [1..] suite]

shouldParse x = parses x @?= True

parserTests =
  [ shouldParse "x :: Int; y :: Int;"
  , shouldParse "x :: Int; procedure foo(y :: Int) { x += y * 42; }"
  , shouldParse "procedure foo() { if x * 3 == 1 then y += 42; else swap x y; fi False; }"
  , shouldParse "procedure foo() { local x :: Int = 42; x += 10; delocal 52; }"
  -- Loop with `do` and `loop`.
  , shouldParse "procedure foo() { from True do x += 1; loop x += 2; until x == 1000; }"
  , shouldParse "procedure foo() { from x * 100 == 400 do x += 1; loop if True then x += 1; else complement x; fi True; until x == 1000; }"
  -- Loop with `do`
  , shouldParse "procedure foo() { from x * 100 == 400 do x += 1; until x == 1000; }"
  -- Loop with `loop`.
  , shouldParse "procedure foo() { from x * 100 == 400 do if True then x += 1; else complement x; fi True; until x == 1000; }"
  , shouldParse "procedure foo() { call f x y; swap x y; complement x; }"
  ]

infix 1 @~>
(@~>) e vars = eVars @?= vars
  where eVars = extractVarsE (unsafePerformIO $ runQ e)

data TestRec = TestRec { x :: Int, y :: Int -> Int }
varTests =
  [ [| x + y |] @~> ["x", "+", "y"]
  , [| foo x y |] @~> ["foo", "x", "y"]
  , [| \x -> x + (y + 1) |] @~> ["+", "y"]
  , [| map (+ x) [y] |] @~> ["map", "+", "x", "y"]
  , [| (x + x) + (1 + (y - 10)) |] @~> ["x", "+", "y", "-"]
  , [| \(x, y) -> x * y |] @~> ["*"]
  , [| if x then x + y else x + z |] @~> ["x", "+", "y", "z"]
  , [| sum [x..(y + 1)] |] @~> ["sum", "x", "y", "+"]
  , [| [x, \y -> y + 1] |] @~> ["x", "+"]
  , [| \y -> x + y :: Int -> Int |] @~> ["x", "+"]
  , [| TestRec { x = x, y = \y -> y + 5} |] @~> ["x", "+"]
  , [| initialRec { y = \x -> x + y} |] @~> ["initialRec", "+", "y"]
  ]

semanticTests = [] {-
  [ semanticCheck progT1 @?= True
  , semanticCheck progT2 @?= True
  , semanticCheck progT3 @?= True
  , semanticCheck progF_noMain @?= False
  , semanticCheck progF1 @?= False
  , semanticCheck progF2 @?= False
  , semanticCheck progF3 @?= False
  , semanticCheck progF4 @?= False
  ]
  where progT1 = Program [ Procedure main [] [asgT] ]
        progF1 = Program [ Procedure main [] [asgF1] ]
        progF2 = Program [ Procedure main [] [asgF2] ]
        progF3 = Program [ Procedure main [] [asgF3] ]
        progF4 = Program []
        progF_noMain = Program [ Procedure notMain [] [asgT] ]
        progT2 = Program [ Procedure main [] [asgT2] ]
        progT3 = Program [ Procedure notMain [] [asgT]
                         , Procedure main [] [asgT] ]
        [x, y, main, notMain] = map Identifier ["x", "y", "main", "notMain"]
        [x', y'] = map (VarE . mkName) ["x", "y"]
        int = ConT $ mkName "Int"
        one = LitE $ IntegerL 1
        plus = VarE '(+)
        index arr = InfixE (Just arr) (VarE '(!!)) (Just one)
        asgT = LHSIdentifier x .+= y'
        asgF1 = LHSIdentifier x .+= x'
        asgF2 =
          LHSArray (LHSIdentifier x) one .+=
          InfixE (Just $ index x') plus (Just one)
        asgF3 = LHSArray (LHSIdentifier x) (index x') .+= one
        asgT2 = LHSArray (LHSIdentifier x) (index y') .+= one
        (.+=) e e' = Assignment "+=" [e] (Just e') -}
