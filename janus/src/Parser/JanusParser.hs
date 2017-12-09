{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Parser.JanusParser where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Core
import Text.ParserCombinators.UU.BasicInstances

parser :: String -> Program
parser s = parse pProgram s