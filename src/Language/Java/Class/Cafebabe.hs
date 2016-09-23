{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Java.Class.Cafebabe(
  CafebabeError(..)
, HasCafebabeError(..)
, AsCafebabeError(..)
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

getCafebabe ::
  AsCafebabeError e =>
  Get e ()
getCafebabe =
  do  c <- _CafebabeUnexpectedEof # () !- word32be 
      unless (c == 0xCAFEBABE) (failGet (_CafebabeInvalidMagicNumber # c))
