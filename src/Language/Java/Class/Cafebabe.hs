{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Java.Class.Cafebabe(
  CafebabeError(..)
, AsCafebabeUnexpectedEof(..)
, cafebabeUnexpectedEof
, AsCafebabeInvalidMagicNumber(..)
, cafebabe
) where

import Control.Applicative(Applicative)
import Control.Lens(Optic', Choice, prism', ( # ))
import Control.Monad(return)
import Data.Eq(Eq((==)))
import Data.Functor.Identity(Identity)
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord)
import Data.Tagged(Tagged)
import Data.Tickle(Get, (!-), word32be, failGet)
import Data.Word(Word32)
import Prelude(Show)

data CafebabeError =
  CafebabeUnexpectedEof
  | CafebabeInvalidMagicNumber Word32
  deriving (Eq, Ord, Show)

class AsCafebabeUnexpectedEof p f s where
  _CafebabeUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsCafebabeUnexpectedEof p f CafebabeError where
  _CafebabeUnexpectedEof =
    prism'
      (\() -> CafebabeUnexpectedEof)
      (\e -> case e of
               CafebabeUnexpectedEof -> Just ()
               CafebabeInvalidMagicNumber _ -> Nothing)

cafebabeUnexpectedEof ::
  AsCafebabeUnexpectedEof Tagged Identity t =>
  t
cafebabeUnexpectedEof =
  _CafebabeUnexpectedEof # ()

class AsCafebabeInvalidMagicNumber p f s where
  _CafebabeInvalidMagicNumber :: 
    Optic' p f s Word32

instance (Choice p, Applicative f) => AsCafebabeInvalidMagicNumber p f CafebabeError where
  _CafebabeInvalidMagicNumber =
    prism'
      CafebabeInvalidMagicNumber
      (\e -> case e of
               CafebabeUnexpectedEof -> Nothing
               CafebabeInvalidMagicNumber w -> Just w)
    
cafebabe ::
  (AsCafebabeUnexpectedEof Tagged Identity e, AsCafebabeInvalidMagicNumber Tagged Identity e) =>
  Get e ()
cafebabe =
  do c <- cafebabeUnexpectedEof !- word32be 
     if c == 0xCAFEBABE
       then
         return ()
       else
         failGet (_CafebabeInvalidMagicNumber # c)         