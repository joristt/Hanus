{-# LANGUAGE QuasiQuotes #-}

module TQQ where

import Test.Framework.Providers.HUnit
import Test.HUnit

import QQ

-- [janusF|examples/test.janus|]

[hanus|
x :: Int;
procedure f(x::Int, y::Int->String) {
 x += 3;
 y ^= 1;
}
|]

qqTests =
  [ decl @?= 10
  , decl' @?= -10
  ]
