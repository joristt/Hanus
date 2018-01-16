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
    n :: Int;

    procedure ifthen()
    {
        n += 10;
        
        if n==10 then
            n += 10;
        fi n==20;
    }
|]
