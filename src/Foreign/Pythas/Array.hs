{- |
Module          : Foreign.Pythas.Array
Description     : Array type for Python's interfacing library Pythas
Copyright       : (c) Simon Plakolb, 2020
License         : LGPLv3
Maintainer      : s.plakolb@gmail.com
Stability       : beta

    The 'Foreign.Pythas.Array' 'CArray' type is the standard way of wrapping Haskell lists in Pythas.
    Lacking a terminator element for many 'Storable' types, array reading has to be constrained by the array length. To communicate the length of an array across language borders it needs to be packed into a struct containing said length. This enables 'peekArray' to safely read the list contents back from a 'CArray'.
    Be aware that while 'String's are '[Char]'s, they are treated seperately in Pythas. See: 'Foreign.Pythas.String'.
 -}
module Foreign.Pythas.Array (CArray, newArray, peekArray, freeArray) where

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types (CInt)
import Foreign.C.Structs (Struct2(..))
import Foreign.Storable (Storable, peek)
import Foreign.Marshal.Utils (new)
import Foreign.Marshal.Alloc (free)
import qualified Foreign.Marshal.Array as ARR (newArray, peekArray)


-- | Type synonym for a pointer to a struct with a field of type 'CInt' containing the length of the array and  a pointer to the first element of the actual 'Foreign.Marshal.Array'.
type CArray a = Ptr (Struct2 CInt (Ptr a))

-- | Allocates space for a 'CArray' while building it from a Haskell list of 'Storable's. The 'CArray' has to be freed after its use with 'freeArray'.
newArray :: (Storable a) => [a] -> IO (CArray a)
newArray xs = ARR.newArray xs >>= new . Struct2 (fromIntegral $ length xs)

-- | (Re-)Creates a Haskell list out of a 'CArray'. Memory is not released within this function. If it had been allocated within Haskell it needs to be freed with 'freeArray'.
peekArray :: (Storable a) => CArray a -> IO [a]
peekArray ap = do
    Struct2 l a <- peek ap
    if a == nullPtr
      then return []
      else ARR.peekArray (fromIntegral l) a

-- | Frees all memory allocated for a 'CArray' by 'newArray'.
freeArray :: (Storable a) => CArray a -> IO ()
freeArray ap = do
    Struct2 _ a <- peek ap
    free a
    free ap

