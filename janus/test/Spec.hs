import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit


main = defaultMain
  [ constructTestSuite testName testSuite
  | (testName, testSuite) <- [ ("DUMMY", dummyTests)
                             ]
  ]

constructTestSuite s suite =
  testGroup s [testCase (s ++ "_" ++ show i) t | (i, t) <- zip [1..] suite]

dummyTests =
  [ "ab" @?= ['a', 'b']
  , 1 + 1 @?= 2
  ]
