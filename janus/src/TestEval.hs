{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, QuasiQuotes #-}

module TestEval where

import AST
import Eval
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import StdLib.Operator

import QQ
import StdLib.Operator

import qualified Data.List as List
import qualified Data.Char as Char

[hanusT|
    x :: Int;

    procedure foo()
    {
        call bar;
    } 

    procedure bar()
    {
        local n :: T = 10;
          n += 10
        delocal n == 20;
    }
|]


