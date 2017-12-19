import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import TParser
import TQQ
import TSemanticChecker

main = defaultMain
  [ constructTestSuite testName testSuite
  | (testName, testSuite) <- [ ("PARSER", parserTests)
                             , ("VARIABLE_EXTRACTION", varTests)
                             , ("SEMANTIC_CHECKING", semanticTests)
                             , ("QUASI_QUOTATION", qqTests)
                             ]
  ]

constructTestSuite s suite =
  testGroup s [testCase (s ++ "_" ++ show i) t | (i, t) <- zip [1..] suite]
