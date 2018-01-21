{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, QuasiQuotes #-}

module TestEval where

import AST
import Eval
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import StdLib.Operator

import QQ
import StdLib.Operator
import StdLib.ArrayIndexer
import StdLib.DefaultValue

import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Map as Map

import Debug.Trace

[hanusT|

    hashmap :: Map.Map String Int;

    procedure testmap()
    {
        hashmap["foo"] += 10 :: Int;
        hashmap["bar"] += 1  :: Int;
        local x :: Int = (indexerGet hashmap "foo") :: Int;
        hashmap["bar"] += x;
        delocal 10;
    }

|]



