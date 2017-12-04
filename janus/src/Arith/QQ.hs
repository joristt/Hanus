{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Arith.QQ where

import Control.Monad
import Data.Generics (extQ)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta (parseExp)

import Arith.AST
import Arith.Parser


-- | Arith quasi-quoter.
arith :: QuasiQuoter
arith = QuasiQuoter {
      quoteExp = \prog -> do
        expr <- runQQParser prog True
        dataToExpQ (const Nothing `extQ` metaVarExp) expr
    , quotePat = \prog -> do
      expr <- runQQParser prog False
      dataToPatQ (const Nothing `extQ` metaVarPat) expr
    , quoteType = undefined
    , quoteDec  = undefined
    }

-- | Arith quasi-quoter for files.
arithF :: QuasiQuoter
arithF = quoteFile arith

-- | Parse program by passing location info and doing semantic checking.
runQQParser :: String -> Bool -> Q Expr
runQQParser prog toCheck = do
  loc <- location
  let pos = (loc_filename loc, fst (loc_start loc), snd (loc_start loc))
  expr <- runParserWithLoc pos prog
  when (toCheck && not (semanticChecker expr)) $ fail "Semantic check failed!"
  return expr

-- | Anti-quotation.
metaVarExp :: Expr -> Maybe (Q Exp)
metaVarExp (MetaVar x) =
  case parseExp x of
    Left err -> fail err
    Right e -> Just [| toExpr $(return e) |]
metaVarExp _ = Nothing

metaVarPat :: Expr -> Maybe (Q Pat)
metaVarPat (MetaVar x) = Just $ varP $ mkName x
metaVarPat _ = Nothing
