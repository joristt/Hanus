{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, QuasiQuotes, FlexibleContexts #-}

module RLE where

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
procedure encode(text :: [Int], arc :: [Int]){
    from (text /= []) && arc == [] do
        local val :: Int = 0;
        local n :: Int = 0;
        val += head text;
        #log val;
        #log text;
        #log arc;
        #log n;
        from n == 0 do
            #log n;
            local tmp :: Int = 0;
            #log tmp;
            pop text tmp;
            #log tmp;
            delocal val;
            n += 1;
            #log n;
        until text == [] || ((head text) /= val);
        push arc val;
        push arc n;
        delocal 0;
        delocal 0;
    until text == [];
}

procedure main(text :: [Int], arc :: [Int]){
    call encode text arc;
}
|]