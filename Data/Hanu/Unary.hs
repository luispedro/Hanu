{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.Hanu.Unary
    ( sqrt
    , exp
    , log
    ) where

import qualified Data.Array.Repa as R
import qualified Prelude as P

sqrt = R.map P.sqrt
exp = R.map P.exp
log = R.map P.log
