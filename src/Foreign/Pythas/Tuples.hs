{-# LANGUAGE TemplateHaskell #-}
{- |
Module          : Foreign.Pythas.Tuples
Description     : Tuple types for Python's interfacing library Pythas
Copyright       : (c) Simon Plakolb, 2020
License         : LGPLv3
Maintainer      : s.plakolb@gmail.com
Stability       : beta

    Tuple wrapper types and converting functions. Implementation strongly relies on 'Foreign.C.Structs'.
    The size of tuples that can be created is directly dependent on the size of structs available there.
 -}
module Foreign.Pythas.Tuples (
    CTuple2, newTuple2, peekTuple2,
    CTuple3, newTuple3, peekTuple3,
    CTuple4, newTuple4, peekTuple4,
    -- Re-Export of free and its aliases to match API of other Pythas-Types
    free, freeTuple2, freeTuple3, freeTuple4
) where

import Foreign.Storable (Storable, peek)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (new)
import Foreign.C.Structs (Struct2(..), Struct3(..), Struct4(..))

import Foreign.Pythas.Templates (ctupleT)

ctupleT 2
ctupleT 3
ctupleT 4
