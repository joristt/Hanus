{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module TQQ where

import Test.Framework.Providers.HUnit
import Test.HUnit

import QQ
import StdLib.Operator

[hanusF|examples/test1.janus|]

[hanusT|
    x :: Int;
    y :: Int;
    z :: Int;

    procedure runtest()
    {
        x += 1;
        y += 2;
        z += 3;
    }
|]

-- | Assert that the quasiquoter is working properly
qqTests =
  [ run @?= (1,2,3)
  ]
