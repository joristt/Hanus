{-# LANGUAGE QuasiQuotes #-}

module TArith where

import Test.Framework.Providers.HUnit
import Test.HUnit

import Arith.QQ
import Arith.Eval

arithTests =
  [ eval [arithF|test/test.arith|] @?= 30
  , eval [arithF|test/test.arith|] @?= eval [arith|
      0 +
      ((2 + 3) +
      (1 + (1 + 1)) + `calcNum 7`)
    |]
  ]
  where calcNum = (+ sum [1..5]) :: Integer -> Integer
