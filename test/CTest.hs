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

-- foreign import ccall "listDouble" listDouble :: CList CDouble
foreign import ccall "listInt" listInt :: CList CInt
-- foreign import ccall "listFloat" listFloat :: CList CFloat

fibs = 1.0 : 2.0 : zipWith (+) fibs (tail fibs) :: [CDouble]
doubles = take 63 fibs :: [CDouble]
ints = [0..41] :: [CInt]
floats = map (/2.0) [0..20] :: [CFloat]

pfa :: Storable a => CArray a -> IO [a]
pfa arr = do
    l <- peekArray arr
    freeArray arr
    return l

pfl :: Storable a => CList a -> IO [a]
pfl list = do
    l <- peekList list
    freeList list
    return l

tests = testGroup "Foreign Imports" [
        testCase "arrayDouble" $ (quick $ pfa arrayDouble) @?= doubles
      , testCase "arrayInt"    $ (quick $ pfa arrayInt)    @?= ints
      , testCase "arrayFloat"  $ (quick $ pfa arrayFloat)  @?= floats
--      , testCase "listDouble"  $ (quick $ pfl listDouble)   @?= doubles
      , testCase "listInt"     $ (quick $ pfl listInt)      @?= ints
--      , testCase "listFloat"   $ (quick $ pfl listFloat )   @?= floats
   ]
