import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import TQQ
import TSemanticChecker
import TEval

main = defaultMain
  [ constructTestSuite testName testSuite
  | (testName, testSuite) <- [ ("VAR", varTests)
                             , ("SEM", semanticTests)
                             , ("QQ", qqTests)
                             , ("EVAL", evalTests)
                             ]
  ]

constructTestSuite s suite =
  testGroup s [testCase (s ++ "_" ++ show i) t | (i, t) <- zip [1..] suite]
