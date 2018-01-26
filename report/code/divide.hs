{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, QuasiQuotes, FlexibleContexts #-}

module Divide where
    
import QQ
import StdLib.Operator
import StdLib.DefaultValue

[hanusT|
procedure divide(x :: Int, y :: Int, z :: Int){
    from x >= y && z == 0 loop
        z += 1;
        x -= y;
    until x < y;
}

procedure main(x :: Int, y :: Int, z :: Int){
    call divide x y z;
    uncall divide x y z;
}|]