{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, QuasiQuotes, FlexibleContexts #-}

module TestEval where

import AST
import Eval
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import StdLib.Operator

import QQ
import StdLib.Operator
import StdLib.ArrayIndexer
import StdLib.FieldIndexer
import StdLib.DefaultValue

import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Map as Map

import Debug.Trace

[hanusT|

    a :: Array Int Int;
    b :: Array String Int;

    procedure testmap()
    {
        a[0] += 10;
        a[1] += 5;

        b["foo"] += 1;
        b["bar"] += 2;
    }

|]

[hanusT|
    t :: (Int, Int);

    procedure testtuple()
    {
        t.first += 10;
        t.second += 100;
    }
|]



