module Main where

import Test.Framework

import Data.Hanu.Tests.NumpyFormat

main = defaultMain
    [Data.Hanu.Tests.NumpyFormat.tests
    ]
