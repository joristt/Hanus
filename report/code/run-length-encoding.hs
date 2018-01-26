{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, QuasiQuotes, FlexibleContexts #-}

module RLE where

import QQ
import StdLib.Operator
import StdLib.DefaultValue

[hanusT|
procedure encode(text :: [Int], arc :: [Int]){
    from (text /= []) && arc == [] do
        local val :: Int = 0;
        local n :: Int = 0;
        val += head text;
        from n == 0 do
            local tmp :: Int = 0;
            pop text tmp;
            delocal val;
            n += 1;
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