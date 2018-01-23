{-# LANGUAGE QuasiQuotes #-}

module TArith where

import Test.Framework.Providers.HUnit
import Test.HUnit

import Arith.QQ

calcNum :: Integer -> Integer
calcNum = (+ sum [1..5])

[arithF|test/test.arith|]

[arith|myProg:
0 +
((2 + 3) +
(1 + (1 + 1)) + `calcNum 7`)
|]

arithTests =
  [ testArith @?= 30
  , testArith' @?= - testArith
  , myProg @?= testArith
  , myProg' @?= testArith'
  ]
