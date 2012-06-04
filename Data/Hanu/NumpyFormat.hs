{-# LANGUAGE ScopedTypeVariables #-}
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

writeArray :: (NumpyDtype a) => V.Vector a -> [Int] -> Put
writeArray array shape = do
        let h = buildHeader (dtypeStr (array V.! 0))
            pad = padding $ length h
        writeMagicNr
        putWord16le $ convert (length h + pad)
        mapM_ (putWord8 . convert) h
        replicateM_ pad (putWord8 32)
        V.mapM_ writeValue array
    where
        writeMagicNr :: Put
        writeMagicNr = mapM_ (putWord8 . convert) "\x93NUMPY\1\0"

        padding :: Int -> Int
        padding n = if r == 0
                        then 0
                        else 16-r
            where
                r = (10 + n) `rem` 16

        buildHeader :: String -> String
        buildHeader descr = concat
                        ["{ 'descr': '"
                        ,descr
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
        _ <- getWord8
        _ <- getWord8
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
