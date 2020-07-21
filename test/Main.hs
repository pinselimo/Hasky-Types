module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified TypeProperties (tests)
import qualified CTest (tests)

main = defaultMain $ testGroup "Tasty" [
                     TypeProperties.tests
--                   On Travis-CI cabal runs into issues allocating memory
--                   when doing the CTest.tests.
--                   SMH it works in the Foreig.C.Types package but not in
--                   this one, which could be due to the bigger allocations.
--
--                   This branch is intended to test out different options.
                   , CTest.tests
                   ]

