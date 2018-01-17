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

    procedure ifthen(y :: Int)
    {
        n += 9;
        if n==10 then
            n += 1;
            if n==10 then
                n -= 1;
            else 
                n += 1;
            fi n==12;
        else
            n -= 1;
        fi n==12;
    }
|]
