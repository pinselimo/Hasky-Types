module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified TypeProperties (tests)

main = defaultMain TypeProperties.tests

