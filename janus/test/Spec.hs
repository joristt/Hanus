{-# LANGUAGE TemplateHaskell #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.QQ

import Data.List ((\\))
import Language.Haskell.TH
import System.IO.Unsafe (unsafePerformIO)

import AST
import SemanticChecker (extractVarsE, semanticCheck)

import Debug.Trace

main = defaultMain
  [ constructTestSuite testName testSuite
  | (testName, testSuite) <- [ ("VAR", varTests)
                             , ("SEM", semanticTests)
                             , ("QQ", qqTests)
                             ]
  ]

constructTestSuite s suite =
  testGroup s [testCase (s ++ "_" ++ show i) t | (i, t) <- zip [1..] suite]

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

semanticTests =
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
        (.+=) e e' = Assignment "+=" [e] (Just e')
