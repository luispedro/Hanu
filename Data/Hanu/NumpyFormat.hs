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

writeArray :: forall a t. (NumpyDtype a, T.Traversable t) => t a -> [Int] -> Put
writeArray array shape = do
        writeMagicNr
        putWord16le $ convert (length header + padding)
        mapM_ (putWord8 . convert) header
        replicateM_ padding (putWord8 32)
        void $ T.mapM writeValue array
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
        skip 2
        n <- getWord16le
        h <- forM [1..n] (\_ -> convert `fmap` getWord8)
        let shape = parseHeader h
        arr <- V.replicateM (product shape) getValue
        return (arr,shape)
    where
        getMagicNr :: Get [Word8]
        getMagicNr = replicateM 6 getWord8
        parseHeader :: String -> [Int]
        parseHeader h = case parse parser "internal" h of
                            Right val -> val
                            Left err -> error $ show err
        parser = (string "{ 'descr': '")
                        *> (string $ dtypeStr (undefined :: a))
                        *> (string "', 'fortran_order': False, 'shape': (")
                        *> (many parseIntPar)
                        <* (string "), }")
                        <* (many (string " "))
        parseIntPar = read `fmap` ((many digit) <* (string ","))
