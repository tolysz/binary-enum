{-# LANGUAGE  ViewPatterns, ScopedTypeVariables, StandaloneDeriving #-}

module Data.Binary.Enum where

import Prelude
import Data.Binary
import Control.Applicative
import qualified Data.List as DL

-- | we can store Enum values inside values we know how to store, so let
newtype BitMap t a = BitMap [a]
  -- ^ Map lets you encode 'flags' compressing distinct eg. packing 8 diferent flags per byte
deriving instance  (Show a) => Show (BitMap t a)

newtype BitEnc t a = BitEnc a
          -- ^ t is a store type, you will need to make sure that your enum fits there
          -- eg. you want to store `Bool` as `Word64` field `BitEnc Word64 Bool`
deriving instance  (Show a) => Show (BitEnc t a)

instance (Enum a, Enum t, Binary t) => Binary (BitEnc t a) where
  put (BitEnc a) = put (toEnum . fromEnum $ a :: t)
  get = BitEnc . toEnum . fromEnum <$> (get :: Get t)

instance (Enum a, Eq a, Eq t, Integral t, Binary t) => Binary (BitMap t a) where
  put a = put ( (toEnum (fromEnum a)) :: t )
  get = toEnum . fromEnum <$> (get :: Get t)

instance (Enum a, Enum t, Eq a, Eq t, Num t) => Enum (BitMap t a) where
  fromEnum (BitMap a) = fromEnum (foldr (\(fromEnum -> x) y -> 2 ^ x + y ) 0 (DL.nub a) :: t)
  toEnum = BitMap . process 0  -- (fromEnum k :: t)
      where
        process _ 0    = []
        process k (flip divMod 2 -> (d, m))
          | m == 1     = toEnum k : process (k + 1) d
          | otherwise  =            process (k + 1) d

instance (Enum a, Enum t, Binary t) => Enum (BitEnc t a) where
  fromEnum (BitEnc a) = fromEnum a
  toEnum = BitEnc . toEnum . fromEnum
