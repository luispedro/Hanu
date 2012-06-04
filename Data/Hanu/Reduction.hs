{-# LANGUAGE TypeOperators, FlexibleContexts, NoMonomorphismRestriction #-}
module Data.Hanu.Reduction
    ( mean
    , var
    , std
    ) where 
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as R
import qualified Data.Vector.Unboxed as VU
import Data.Hanu.Unary
import Prelude hiding (sqrt, sum)


sum :: (R.Shape sh, R.Elt a, Num a, R.Repr r a, VU.Unbox a) => R.Array r (sh R.:. Int) a -> R.Array R.U sh a
sum = R.sumS

mean :: (R.Shape sh, R.Repr r Double) => R.Array r (sh R.:. Int) Double -> R.Array R.D sh Double
mean arr = (sum arr) R./^ n'
    where
        shead R.:. n = R.extent arr
        n' = sc shead $ fromIntegral n

var :: (R.Shape sh, R.Repr r Double) => R.Array r (sh R.:. Int) Double -> R.Array R.D sh Double
var arr = s2 R.-^ (mu **^ (sc shead 2))
    where
        s2 = sum $ R.map (** 2) arr
        shead R.:. _ = R.extent arr
        mu = mean arr

(**^) = R.zipWith (**)

std :: (R.Shape sh, R.Repr r Double) => R.Array r (sh R.:. Int) Double -> R.Array R.D sh Double
std = sqrt . var

sc sh a = R.fromFunction sh (const a)
