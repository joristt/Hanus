module TParser where

import Test.Framework.Providers.HUnit
import Test.HUnit

import AST
import Parser.JanusParser

shouldParse x = parses x @?= True

parserTests =
  [ shouldParse "x :: Int; y :: Int;"
  , shouldParse "x :: Int; procedure foo(y :: Int) { x += y * 42; }"
  , shouldParse "procedure foo() { if x * 3 == 1 then y += 42; else swap x y; fi False; }"
  , shouldParse "procedure foo() { local x :: Int = 42; x += 10; delocal 52; }"
  -- Loop with `do` and `loop`.
  , shouldParse "procedure foo() { from True do x += 1; loop x += 2; until x == 1000; }"
  , shouldParse "procedure foo() { from x * 100 == 400 do x += 1; loop if True then x += 1; else complement x; fi True; until x == 1000; }"
  -- Loop with `do`
  , shouldParse "procedure foo() { from x * 100 == 400 do x += 1; until x == 1000; }"
  -- Loop with `loop`.
  , shouldParse "procedure foo() { from x * 100 == 400 do if True then x += 1; else complement x; fi True; until x == 1000; }"
  , shouldParse "procedure foo() { call f x y; swap x y; complement x; }"
  ]
