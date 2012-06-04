{-# LANGUAGE ScopedTypeVariables, ConstraintKinds #-}
module Data.Hanu.NumpyFormat
    ( readArray
    , writeArray
    ) where
import Text.Parsec
import Data.Binary
import Data.Binary.IEEE754
import Data.Binary.Put
import Data.Binary.Get
import Data.Convertible
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Control.Applicative hiding (many)
import Control.Monad
import qualified Data.Vector as V

class NumpyDtype dtype where
    writeValue :: dtype -> Put
    getValue :: Get dtype
    dtypeStr :: dtype -> String -- This actually ignores its input argument

instance NumpyDtype Double where
    writeValue = putFloat64le
    getValue = getFloat64le
    dtypeStr = (const "<f8")

instance NumpyDtype Float where
    writeValue = putFloat32le
    getValue = getFloat32le
    dtypeStr = (const "<f4")

instance NumpyDtype Int where
    writeValue = putWord32le . convert
    getValue = convert `fmap` getWord32le
    dtypeStr = (const "<i4")

writeArray :: forall a t. (NumpyDtype a, T.Traversable t) => t a -> [Int] -> Put
writeArray array shape
    |length shape >= 32 = error "Array dimension must be less than 32"
    |otherwise = do
        writeMagicNr
        putWord16le $ convert (length header + padding)
        mapM_ (putWord8 . convert) header
        replicateM_ padding (putWord8 32)
        written <- T.mapM writeValue array
        when ((length $ F.toList written) /= product shape)
            (error "Number of elements does not match")
    where
        writeMagicNr :: Put
        writeMagicNr = mapM_ (putWord8 . convert) "\x93NUMPY\1\0"

        padding :: Int
        padding = if r == 0
                        then 0
                        else 16-r
            where
                r = (10 + length header) `rem` 16
        header :: String
        header = concat
                    ["{ 'descr': '"
                    ,dtypeStr (undefined :: a)
                    ,"', 'fortran_order': False, "
                    ,"'shape': ("
                    ,asPythonArray shape
                    ,", }"]
            where
                asPythonArray [] = ""
                asPythonArray [a] = concat [show a, ",)"]
                asPythonArray (a:as) = (concat [show a, ","]) ++ asPythonArray as

readArray :: forall a. NumpyDtype a => Get (V.Vector a, [Int])
readArray = do
        _magic <- getMagicNr
        major <- getWord8
        minor <- getWord8
        when (major /= 1 || minor /= 0)
            (error "Format version cannot be handled by this version of Hanu")
        n <- getWord16le
        h <- forM [1..n] (\_ -> convert `fmap` getWord8)
        case parseHeader h of
            Right shape -> do
                arr <- V.replicateM (product shape) getValue
                return (arr,shape)
            Left err -> fail (show err)
    where
        getMagicNr :: Get [Word8]
        getMagicNr = replicateM 6 getWord8
        parseHeader h = parse parser "internal" h
        parser = (string "{ 'descr': '")
                        *> (string $ dtypeStr (undefined :: a))
                        *> (string "', 'fortran_order': False, 'shape': (")
                        *> (many parseIntPar)
                        <* (string "), }")
                        <* (many (string " "))
        parseIntPar = read `fmap` ((many digit) <* (string ","))
