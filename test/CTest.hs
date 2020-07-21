{-# LANGUAGE ForeignFunctionInterface #-}
module CTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import System.IO.Unsafe (unsafePerformIO)

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Hasky.Array
import Foreign.Hasky.List

quick = unsafePerformIO

foreign import ccall "arrayDouble" arrayDouble :: CArray CDouble
foreign import ccall "arrayInt" arrayInt :: CArray CInt
foreign import ccall "arrayFloat" arrayFloat :: CArray CFloat
foreign import ccall "listDouble" listDouble :: CList CDouble
foreign import ccall "listInt" listInt :: CList CInt
foreign import ccall "listFloat" listFloat :: CList CFloat

fibs = 1.0 : 2.0 : zipWith (+) fibs (tail fibs) :: [CDouble]
doubles = take 63 fibs :: [CDouble]
ints = [0..41] :: [CInt]
floats = map (/2.0) [0..20] :: [CFloat]

tests = testGroup "Foreign Imports" [
        testCase "arrayDouble" $ (quick $ peekArray arrayDouble) @?= doubles,
        testCase "arrayInt"    $ (quick $ peekArray arrayInt)    @?= ints,
        testCase "arrayFloat"  $ (quick $ peekArray arrayFloat)  @?= floats,
        testCase "listDouble"  $ (quick $ peekList listDouble)   @?= doubles,
        testCase "listInt"     $ (quick $ peekList listInt)      @?= ints,
        testCase "listFloat"   $ (quick $ peekList listFloat )   @?= floats
    ]
