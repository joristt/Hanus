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
    y :: Int;
    stack :: [Int];

    procedure stacktest()
    {
        x += 1;
        push stack x;
        push stack y;
        x += 100;
        push stack x;
        pop stack y;
    }
|]


