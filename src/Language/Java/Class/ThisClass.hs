{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Java.Class.ThisClass {- (
  ThisClass(..)
, ThisClassError(..)
, AsThisClassUnexpectedEof(..)
, thisClassUnexpectedEof
, thisClass
) -} where

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
newtype ThisClass =
  ThisClass Word16
  deriving (Eq, Ord, Show)

data ThisClassError =
  ThisClassUnexpectedEof
  deriving (Eq, Ord, Show)

{-
class AsThisClassUnexpectedEof p f s where
  _ThisClassUnexpectedEof :: 
    Optic' p f s ()

instance (Profunctor p, Functor f) => AsThisClassUnexpectedEof p f ThisClassError where
  _ThisClassUnexpectedEof =
    iso
      (\_ -> ())
      (\() -> ThisClassUnexpectedEof)

thisClassUnexpectedEof ::
  AsThisClassUnexpectedEof Tagged Identity t =>
  t
thisClassUnexpectedEof =
  _ThisClassUnexpectedEof # ()

thisClass ::
  AsThisClassUnexpectedEof Tagged Identity e =>
  Get e ThisClass
thisClass =
  do af <- thisClassUnexpectedEof !- word16be
     return (ThisClass af)
-}