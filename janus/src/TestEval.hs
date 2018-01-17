{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, QuasiQuotes #-}

module TestEval where

import AST
import Eval
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import StdLib.Operator

import QQ
import StdLib.Operator


[hanusT|
    n :: Integer;

    procedure n()
    {
        n += 5;
        from n = 5
            do
                n -= 1;
            loop
        until n == 1;
    }
|]
