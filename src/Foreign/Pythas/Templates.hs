{-# LANGUAGE TemplateHaskell, CPP #-}
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
    ctupleT
) where

import Language.Haskell.TH

import Foreign.Storable (Storable, peek)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (new)
import Foreign.C.Structs (Struct2(..), Struct3(..), Struct4(..))

ctupleT :: Int -> DecsQ
ctupleT n  = do
    ft <- freeT n
    let tt = typT n
        nt = newTupleT n
        pt = peekTupleT n
    return $ [tt,nt,pt] ++ ft

-- Template functions
typT :: Int -> Dec
typT n = TySynD (ctuple n) (map PlainTV vs) typ
    where typ = AppT (ConT ''Ptr) $ foldl AppT (ConT $ struct n) (map VarT vs)
          vs  = vars n

freeT :: Int -> DecsQ
freeT n = [d| $(name) = free |]
    where name = varP $ nName "freeTuple" n

newTupleT :: Int -> Dec
newTupleT n = FunD (nName "newTuple" n) [clause]
    where clause = Clause [TupP $ map VarP vs] (NormalB body) []
          body = AppE (VarE 'new) (foldl AppE (ConE $ struct n) $ map VarE vs)
          vs = vars n

peekTupleT :: Int -> Dec
peekTupleT n = FunD (nName "peekTuple" n) [clause]
    where clause = Clause [VarP ct] (NormalB body) []
          ct = mkName "ct"
          strP = ConP (struct n) $ map VarP vs
          vs = vars n
          body = DoE [ BindS (strP) (AppE (VarE 'peek) (VarE ct))
                     , NoBindS (AppE (VarE 'return) (TupE fields))
                     ]
                     where
#if __GLASGOW_HASKELL__ < 810
                           fields = map VarE vs
#else
                           fields = map (Just . VarE) vs
#endif

-- Utility functions
ctuple n = nName "CTuple" n
struct n = nName "Struct" n
nName s n = mkName $ s ++ show n
vars   n = take n $ map (mkName . (:[])) ['a'..'z']

