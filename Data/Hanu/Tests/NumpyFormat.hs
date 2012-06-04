{-# LANGUAGE TemplateHaskell #-}
module Data.Hanu.Tests.NumpyFormat
    ( tests ) where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit

import qualified Data.Vector as V
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import Data.Hanu.NumpyFormat

tests = $(testGroupGenerator)

case_read_write = v `veq` arr @? "read/write error"
    where
        arr = (V.fromList $ take 64 $ cycle [1.2,2.4,3.4,0.5]) :: V.Vector Double
        serialized = runPut $ writeArray arr [4,16]
        (v,sh) = (runGet readArray serialized) :: (V.Vector Double,[Int])

case_read_write_int = v `veq` arr @? "read/write error"
    where
        arr = (V.fromList $ take 64 $ cycle [1,2,2,4,3,4,0,5]) :: V.Vector Int
        serialized = runPut $ writeArray arr [4,16]
        (v,sh) = (runGet readArray serialized) :: (V.Vector Int,[Int])

veq a b = (V.length a == V.length b) &&
            (V.foldl (&&) True (V.zipWith (==) a b))

