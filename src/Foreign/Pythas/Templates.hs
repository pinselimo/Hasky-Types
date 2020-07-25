{-# LANGUAGE TemplateHaskell #-}
{- |
Module          : Foreign.Pythas.Templates
Description     : Tuple types for Python's interfacing library Pythas
Copyright       : (c) Simon Plakolb, 2020
License         : LGPLv3
Maintainer      : s.plakolb@gmail.com
Stability       : beta

    Tuple wrapper types and converting functions. Implementation strongly relies on 'Foreign.C.Structs'.
    The size of tuples that can be created is directly dependent on the size of structs available there.
 -}
module Foreign.Pythas.Templates (
    typT
) where

import Language.Haskell.TH

import Foreign.Storable (Storable, peek)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (new)
import Foreign.C.Structs (Struct2(..), Struct3(..), Struct4(..))


ctuple n = mkName $ "CTuple" ++ show n
struct n = mkName $ "Struct" ++ show n

typT n = return $ [TySynD (ctuple n) (map PlainTV var) typ]
    where var = take n $ map (mkName . (:[])) ['a'..'z']
          typ = AppT (ConT ''Ptr) $ foldl AppT (ConT $ struct n) (map VarT var)
