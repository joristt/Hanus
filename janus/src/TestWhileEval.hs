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

import Debug.Trace

[hanusT|
    n :: Int;
    a :: Int;
    b :: Int;

    procedure fib()
    {
        if n == 0 then
            a += 1;
            b += 1;
        else
            n -= 1;
            call fib;
            #debug n a b;
            a += b;
            swap b a;
        fi 1 == 1;
    }
|]
