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

    a :: Array String Int;

    procedure y()
    {
        a["foo"] += 1;
        a["bar"] += 2;
    }

|]

[hanusT|
    t :: (Int, Int);

    procedure x()
    {
        t.first += 10;
        t.second += 100;
        swap t.first t.second;
    }
|]



