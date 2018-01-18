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

    m :: Int;

    procedure fib(n :: Int, xa :: Int, xb :: Int)
    {
        if n==0 then
            xa += 1;
            xb += 1;
        else
            n -= 1;
            call fib n xa xb;
            xa += xb;
            swap xa xb;
        fi xa==xb;
    }

    procedure runfib()
    {
        local n  :: Int = 4;
        local xa :: Int = 0;
        local xb :: Int = 0;
            call fib n xa xb;
            #log n xa xb;
        delocal -4;
        delocal -4;
        delocal -4;
    }

|]
