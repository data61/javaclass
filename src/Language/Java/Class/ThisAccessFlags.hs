{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Java.Class.ThisAccessFlags(
  ThisAccessFlags(..)
, ThisAccessFlagsError(..)
, AsThisAccessFlagsUnexpectedEof(..)
, thisAccessFlagsUnexpectedEof
, thisAccessFlags
) where

import Control.Lens(Optic', Profunctor, iso, ( # ))
import Control.Monad(return)
import Data.Eq(Eq)
import Data.Functor(Functor)
import Data.Functor.Identity(Identity)
import Data.Ord(Ord)
import Data.Tagged(Tagged)
import Data.Tickle(Get, word16be, (!-))
import Data.Word(Word16)
import Prelude(Show)

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1 4.1. The ClassFile Structure>
newtype ThisAccessFlags =
  ThisAccessFlags Word16
  deriving (Eq, Ord, Show)

data ThisAccessFlagsError =
  ThisAccessFlagsUnexpectedEof
  deriving (Eq, Ord, Show)

class AsThisAccessFlagsUnexpectedEof p f s where
  _ThisAccessFlagsUnexpectedEof :: 
    Optic' p f s ()

instance (Profunctor p, Functor f) => AsThisAccessFlagsUnexpectedEof p f ThisAccessFlagsError where
  _ThisAccessFlagsUnexpectedEof =
    iso
      (\_ -> ())
      (\() -> ThisAccessFlagsUnexpectedEof)

thisAccessFlagsUnexpectedEof ::
  AsThisAccessFlagsUnexpectedEof Tagged Identity t =>
  t
thisAccessFlagsUnexpectedEof =
  _ThisAccessFlagsUnexpectedEof # ()

thisAccessFlags ::
  AsThisAccessFlagsUnexpectedEof Tagged Identity e =>
  Get e ThisAccessFlags
thisAccessFlags =
  do af <- thisAccessFlagsUnexpectedEof !- word16be
     return (ThisAccessFlags af)
