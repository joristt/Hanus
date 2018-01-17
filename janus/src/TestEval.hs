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
    {local x :: Int = 10;
            call bar x;
        delocal 10;}

    procedure bar(y :: Int)
    {
        n += y;
        if n==10 then
            y += 1;
        else
            n -= 1;
        fi n==10;
        n += y;
    }
|]
