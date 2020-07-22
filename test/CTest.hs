{-# LANGUAGE ForeignFunctionInterface #-}
module CTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc (free)
import Foreign.Hasky.Array
import Foreign.Hasky.List

check list action = do
    list' <- action
    list @?= list'

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

pfa :: Storable a => CArray a -> IO [a]
pfa arr = do
    l <- peekArray arr
    freeArray arr
    return l

pfl :: Storable a => CList a -> IO [a]
pfl list = do
    l <- peekList list
    free list
    return l

tests = testGroup "Foreign Imports" [
        testCase "arrayDouble" $ check doubles (pfa arrayDouble)
      , testCase "arrayInt"    $ check ints    (pfa arrayInt)
      , testCase "arrayFloat"  $ check floats  (pfa arrayFloat)
      , testCase "listDouble"  $ check doubles (pfl listDouble)
      , testCase "listInt"     $ check ints    (pfl listInt)
--      , testCase "listFloat"   $ (quick $ pfl listFloat )   @?= floats
   ]
