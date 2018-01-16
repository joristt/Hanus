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
    x :: Int;

    procedure foo()
    {
        call bar x;
    }

    procedure bar()
    {
        y += 1;
        x += 1;
    }
|]
