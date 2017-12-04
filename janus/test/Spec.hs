import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import TArith

main = defaultMain
  [ constructTestSuite testName testSuite
  | (testName, testSuite) <- [ ("ARITH", arithTests)
                             ]
  ]

constructTestSuite s suite =
  testGroup s [testCase (s ++ "_" ++ show i) t | (i, t) <- zip [1..] suite]
