import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.List ((\\))
import Language.Haskell.TH
import System.IO.Unsafe (unsafePerformIO)

import AST
import Parser.JanusParser
import SemanticChecker (extractVarsE, semanticCheck)

import TParser
import TQQ
import TSemanticChecker

import Debug.Trace

main = defaultMain
  [ constructTestSuite testName testSuite
  | (testName, testSuite) <- [ ("PARSER", parserTests)
                             , ("VARIABLE_EXTRACTION", varTests)
                             , ("SEMANTIC_CHECKING", semanticTests)
                             , ("QUASI_QUOTATION",  qqTests)
                             ]
  ]

constructTestSuite s suite =
  testGroup s [testCase (s ++ "_" ++ show i) t | (i, t) <- zip [1..] suite]
