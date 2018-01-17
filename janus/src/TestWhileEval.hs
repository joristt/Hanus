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
    m :: Int;

    procedure n(x :: Bool)
    {
        #debug x;
        x += 1;
        #debug x;
    }
|]


