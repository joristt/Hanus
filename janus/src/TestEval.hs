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
    x :: Int;

    procedure foo()
    {
        call bar;
    } 

procedure bar() {x += 10; local n :: Int = 42;n += 10;x += n;delocal 52;}
|]


