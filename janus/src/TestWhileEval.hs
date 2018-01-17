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
    n :: Int;

    procedure foo()
    {
    }

    procedure n()
    {
        n += 1;
        from n == 1
            do n += 1;
            loop n += 1;
        until n == 1;
        n += 1;
    }
|]


