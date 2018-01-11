{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, TemplateHaskell #-}
module TQQ where

import Test.Framework.Providers.HUnit
import Test.HUnit

import QQ
import StdLib.Operator

[hanusF|examples/test1.janus|]

qqTests =
  [ run @?= (1,2,3)
  ]
