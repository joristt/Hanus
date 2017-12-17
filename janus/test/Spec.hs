{-# LANGUAGE TemplateHaskell #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.List ((\\))
import System.IO.Unsafe (unsafePerformIO)
import Language.Haskell.TH

import AST
import SemanticChecker (extractVarsE, semanticCheck)

import Debug.Trace

main = defaultMain
  [ constructTestSuite testName testSuite
  | (testName, testSuite) <- [ ("VAR", varTests)
                             , ("SEM", semanticTests)
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
  [ semanticCheck progT @?= True
  , semanticCheck progF @?= False
  ]
  where progT = Program
          [ GlobalVarDeclaration $ Variable x int
          , Procedure p [Variable y int]
              [ Assignement "+=" [LHSIdentifier x] (VarE $ mkName "y") ]
          ]
        progF = Program
          [ GlobalVarDeclaration $ Variable x int
          , Procedure p [Variable y int]
              [ Assignement "+=" [LHSIdentifier x] (VarE $ mkName "x") ]
          ]
        [x, y, p] = map Identifier ["x", "y", "p"]
        int = ConT $ mkName "Int"
