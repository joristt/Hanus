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

    procedure while()
    {

    }

    procedure n()
    {
        n += 5;
        from n == 0
            do n += 1;
            loop n += 1;
        until n == 1;
        n += 1;
    }
|]
