{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Java.Class.SuperClass(
  SuperClass(..)
, SuperClassError(..)
, AsSuperClassUnexpectedEof(..)
, superClassUnexpectedEof
, superClass
) where

import Control.Lens(Optic', Profunctor, iso, ( # ))
import Control.Monad(Monad(return))
import Data.Eq(Eq)
import Data.Functor(Functor)
import Data.Functor.Identity(Identity)
import Data.Ord(Ord)
import Data.Tagged(Tagged)
import Data.Tickle((!-), Get, word16be)
import Data.Word(Word16)
import Prelude(Show)

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1 4.1. The ClassFile Structure>
newtype SuperClass =
  SuperClass Word16
  deriving (Eq, Ord, Show)

data SuperClassError =
  SuperClassUnexpectedEof
  deriving (Eq, Ord, Show)

class AsSuperClassUnexpectedEof p f s where
  _SuperClassUnexpectedEof :: 
    Optic' p f s ()

instance (Profunctor p, Functor f) => AsSuperClassUnexpectedEof p f SuperClassError where
  _SuperClassUnexpectedEof =
    iso
      (\_ -> ())
      (\() -> SuperClassUnexpectedEof)

superClassUnexpectedEof ::
  AsSuperClassUnexpectedEof Tagged Identity t =>
  t
superClassUnexpectedEof =
  _SuperClassUnexpectedEof # ()

superClass ::
  AsSuperClassUnexpectedEof Tagged Identity e =>
  Get e SuperClass
superClass =
  do af <- superClassUnexpectedEof !- word16be
     return (SuperClass af)
