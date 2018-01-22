{-# LANGUAGE QuasiQuotes #-}
module TQQ where

import Test.Framework.Providers.HUnit
import Test.HUnit

import QQ

[hanusF|examples/test1.janus|]

qqTests =
  [ prog @?= True
  , coProg @?= False
  ]
