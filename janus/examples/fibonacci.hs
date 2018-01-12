{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, QuasiQuotes, FlexibleContexts #-}

module Fibonacci where

import QQ

import StdLib.Operator
import StdLib.DefaultValue

[hanusT|
procedure fib(x1 :: Int, x2 :: Int, n :: Int)
{
    if n == 0 then
        x1 += 1;
        x2 += 1;
    else
        n -= 1;
        call fib x1 x2 n;
        x1 += x2;
        swap x1 x2;
    fi x1 == x2;

}

procedure main(x1 :: Int, x2 :: Int, n :: Int){
    call fib x1 x2 n;
}
|]
