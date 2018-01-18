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

    procedure fibiterlocal()
    {
        local fa :: Int = 1;
        local fb :: Int = 1;

        from fa == fb
            do
                n -= 1;
                fa += fb;
                swap fb fa;
        until n == 1;

        a += fa;
        b += fb;

        delocal 1;
        delocal 1;
    }

    procedure fibiter()
    {
        a += 1;
        b += 1;
        from a == b
            do
                n -= 1;
                a += b;
                swap b a;
        until n == 1;
    }

    procedure fibrec()
    {
        if n == 1 then
            a += 1;
            b += 1;
        else
            n -= 1;
            call fibrec;
            a += b;
            swap b a;
        fi 1 == 1;
    }

|]
