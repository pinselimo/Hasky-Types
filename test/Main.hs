module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified TypeProperties (tests)
import qualified CTest (tests)

main = defaultMain $ testGroup "Tasty" [
                     TypeProperties.tests
                   , CTest.tests
                   ]

