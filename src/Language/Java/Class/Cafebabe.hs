{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Java.Class.Cafebabe(
  CafebabeError(..)
, HasCafebabeError(..)
, AsCafebabeError(..)
, cafebabeUnexpectedEof
, cafebabeInvalidMagicNumber
, getCafebabe
) where

import Control.Monad(unless)
import Data.Eq(Eq((==)))
import Data.Tickle(Get, (!-), word32be, failGet)
import Data.Word(Word32)
import Papa

data CafebabeError =
  CafebabeUnexpectedEof
  | CafebabeInvalidMagicNumber Word32
  deriving (Eq, Ord, Show)

makeClassy ''CafebabeError
makeClassyPrisms ''CafebabeError

cafebabeUnexpectedEof ::
  AsCafebabeError t =>
  t
cafebabeUnexpectedEof =
  _CafebabeUnexpectedEof # ()

cafebabeInvalidMagicNumber ::
  AsCafebabeError t =>
  Word32
  -> t
cafebabeInvalidMagicNumber n =
  _CafebabeInvalidMagicNumber # n

getCafebabe ::
  AsCafebabeError e =>
  Get e ()
getCafebabe =
  do  c <- cafebabeUnexpectedEof !- word32be 
      unless (c == 0xCAFEBABE) (failGet (cafebabeInvalidMagicNumber c))
