{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Java.Class.Version(
  Version(..)
, VersionError(..)
, AsVersionMinorUnexpectedEof(..)
, versionMinorUnexpectedEof
, AsVersionMajorUnexpectedEof(..)
, versionMajorUnexpectedEof
, version
) where

import Control.Applicative(Applicative)
import Control.Lens(Choice, Optic', prism', ( # ))
import Control.Monad(return)
import Data.Eq(Eq)
import Data.Functor.Identity(Identity)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Data.Tagged(Tagged)
import Data.Tickle(Get, word16be, (!-))
import Data.Word(Word16)
import Prelude(Show)

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1 4.1. The ClassFile Structure>
data Version =
  Version
    Word16
    Word16
  deriving (Eq, Ord, Show)

data VersionError =
  VersionMinorUnexpectedEof
  | VersionMajorUnexpectedEof
  deriving (Eq, Ord, Show)

class AsVersionMinorUnexpectedEof p f s where
  _VersionMinorUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsVersionMinorUnexpectedEof p f VersionError where
  _VersionMinorUnexpectedEof =
    prism'
      (\() -> VersionMinorUnexpectedEof)
      (\e -> case e of
               VersionMinorUnexpectedEof -> Just ()
               VersionMajorUnexpectedEof -> Nothing)

versionMinorUnexpectedEof ::
  AsVersionMinorUnexpectedEof Tagged Identity t =>
  t
versionMinorUnexpectedEof =
  _VersionMinorUnexpectedEof # ()

class AsVersionMajorUnexpectedEof p f s where
  _VersionMajorUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsVersionMajorUnexpectedEof p f VersionError where
  _VersionMajorUnexpectedEof =
    prism'
      (\() -> VersionMajorUnexpectedEof)
      (\e -> case e of
               VersionMinorUnexpectedEof -> Nothing
               VersionMajorUnexpectedEof -> Just ())

versionMajorUnexpectedEof ::
  AsVersionMajorUnexpectedEof Tagged Identity t =>
  t
versionMajorUnexpectedEof =
  _VersionMajorUnexpectedEof # ()

version ::
  (AsVersionMinorUnexpectedEof Tagged Identity e, AsVersionMajorUnexpectedEof Tagged Identity e) => 
  Get e Version
version =
  do mn <- versionMinorUnexpectedEof !- word16be
     mj <- versionMajorUnexpectedEof !- word16be
     return (Version mn mj)
