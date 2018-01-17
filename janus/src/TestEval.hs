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
    xa :: Int;
    xb :: Int;

    procedure fib(n :: Int)
    {
        if n==0 then
            xa += 1;
            xb += 1;
        else
            n -= 1;
            call fib n;
            xa += xb;
            swap xa xb;
        fi xa==xb;
    }

procedure runfib(){local n :: Int = 100000;call fib n;delocal 4;}
|]
