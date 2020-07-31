module TypeProperties where

import Test.Tasty.QuickCheck (Property, Arbitrary, arbitrary, testProperty)
import Test.Tasty (testGroup)
import Test.QuickCheck.Monadic (monadicIO, run, assert)

import Foreign.C.Types (CChar, CInt, CDouble, CLLong)
import Foreign.Storable (Storable)

import Foreign.Pythas.Array
import Foreign.Pythas.List
import Foreign.Pythas.Tuples

tests = testGroup "Properties" [test_idArray, test_idList, test_idTuple]

test_idArray = testGroup "Identity Array" [
      testProperty "Double"   (prop_idArray :: [Double]  -> Property)
    , testProperty "Int"      (prop_idArray :: [Int]     -> Property)
    , testProperty "Char"     (prop_idArray :: [Char]    -> Property)
    , testProperty "CDouble"  (prop_idArray :: [CDouble] -> Property)
    , testProperty "CInt"     (prop_idArray :: [CInt]    -> Property)
    , testProperty "CChar"    (prop_idArray :: [CChar]   -> Property)
    , testProperty "CLLong"   (prop_idArray :: [CLLong]  -> Property)
    ]

identityArray list = do
    ptr_array <- newArray list
    list'     <- peekArray ptr_array
    freeArray ptr_array
    return list'

prop_idArray :: (Storable a, Eq a) => [a] -> Property
prop_idArray list = monadicIO $ do
    list' <- run (identityArray list)
    assert (list' == list)

test_idList = testGroup "Identity List" [
      testProperty "Double"   (prop_idList :: [Double]  -> Property)
    , testProperty "Int"      (prop_idList :: [Int]     -> Property)
    , testProperty "Char"     (prop_idList :: [Char]    -> Property)
    , testProperty "CDouble"  (prop_idList :: [CDouble] -> Property)
    , testProperty "CInt"     (prop_idList :: [CInt]    -> Property)
    , testProperty "CChar"    (prop_idList :: [CChar]   -> Property)
    , testProperty "CLLong"   (prop_idList :: [CLLong]  -> Property)
    ]

identityList list = do
    ptr_list <- newList list
    list'    <- peekList ptr_list
    freeList ptr_list
    return list'

prop_idList :: (Storable a, Eq a) => [a] -> Property
prop_idList list = monadicIO $ do
    list' <- run (identityList list)
    assert (list' == list)

test_idTuple = testGroup "Identity Tuple2" [
      testProperty "Double"   (prop_idTuple :: (Double, Double, Double)    -> Property)
    , testProperty "Int"      (prop_idTuple :: (Int, Int, Int)             -> Property)
    , testProperty "Char"     (prop_idTuple :: (Char, Char, Char)          -> Property)
    , testProperty "CDouble"  (prop_idTuple :: (CDouble, CDouble, CDouble) -> Property)
    , testProperty "CInt"     (prop_idTuple :: (CInt, CInt, CInt)          -> Property)
    , testProperty "CChar"    (prop_idTuple :: (CChar, CChar, CChar)       -> Property)
    , testProperty "CLLong"   (prop_idTuple :: (CLLong, CLLong, CLLong)    -> Property)
    ]

identityTuple tuple = do
    ptr_tuple <- newTuple3 tuple
    tuple'    <- peekTuple3 ptr_tuple
    free ptr_tuple
    return tuple'

prop_idTuple :: (Storable a, Eq a) => (a,a,a) -> Property
prop_idTuple tuple = monadicIO $ do
    tuple' <- run (identityTuple tuple)
    assert (tuple' == tuple)

