{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Java.Class.FieldAccessFlags(
  FieldAccessFlags(..)
, FieldAccessFlagsError(..)
, AsFieldAccessFlagsUnexpectedEof(..)
, fieldAccessFlagsUnexpectedEof
, fieldAccessFlags
) where

import Control.Lens(Optic', Profunctor, iso, ( # ))
import Control.Monad(return)
import Data.Eq(Eq)
import Data.Functor(Functor)
import Data.Functor.Identity(Identity)
import Data.Ord(Ord)
import Data.Tagged(Tagged)
import Data.Tickle(Get, (!-), word16be)
import Data.Word(Word16)
import Prelude(Show)

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.5 4.5. Fields>
newtype FieldAccessFlags =
  FieldAccessFlags
    Word16
  deriving (Eq, Ord, Show)

data FieldAccessFlagsError =
  FieldAccessFlagsUnexpectedEof
  deriving (Eq, Ord, Show)

class AsFieldAccessFlagsUnexpectedEof p f s where
  _FieldAccessFlagsUnexpectedEof :: 
    Optic' p f s ()

instance (Profunctor p, Functor f) => AsFieldAccessFlagsUnexpectedEof p f FieldAccessFlagsError where
  _FieldAccessFlagsUnexpectedEof =
    iso
      (\_ -> ())
      (\() -> FieldAccessFlagsUnexpectedEof)

fieldAccessFlagsUnexpectedEof ::
  AsFieldAccessFlagsUnexpectedEof Tagged Identity t =>
  t
fieldAccessFlagsUnexpectedEof =
  _FieldAccessFlagsUnexpectedEof # ()

fieldAccessFlags ::
  AsFieldAccessFlagsUnexpectedEof Tagged Identity e =>
  Get e FieldAccessFlags
fieldAccessFlags =
  do af <- fieldAccessFlagsUnexpectedEof !- word16be
     return (FieldAccessFlags af)
