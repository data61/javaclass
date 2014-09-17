{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Java.Class.MethodAccessFlags(
  MethodAccessFlags(..)
, MethodAccessFlagsError(..)
, AsMethodAccessFlagsUnexpectedEof(..)
, methodAccessFlagsUnexpectedEof
, methodAccessFlags
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
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.6 4.6. Methods>
newtype MethodAccessFlags =
  MethodAccessFlags
    Word16
  deriving (Eq, Ord, Show)

data MethodAccessFlagsError =
  MethodAccessFlagsUnexpectedEof
  deriving (Eq, Ord, Show)

class AsMethodAccessFlagsUnexpectedEof p f s where
  _MethodAccessFlagsUnexpectedEof :: 
    Optic' p f s ()

instance (Profunctor p, Functor f) => AsMethodAccessFlagsUnexpectedEof p f MethodAccessFlagsError where
  _MethodAccessFlagsUnexpectedEof =
    iso
      (\_ -> ())
      (\() -> MethodAccessFlagsUnexpectedEof)

methodAccessFlagsUnexpectedEof ::
  AsMethodAccessFlagsUnexpectedEof Tagged Identity t =>
  t
methodAccessFlagsUnexpectedEof =
  _MethodAccessFlagsUnexpectedEof # ()

methodAccessFlags ::
  AsMethodAccessFlagsUnexpectedEof Tagged Identity e =>
  Get e MethodAccessFlags
methodAccessFlags =
  do af <- methodAccessFlagsUnexpectedEof !- word16be
     return (MethodAccessFlags af)
