{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Java.Class.Version(
  Version(..)
, HasVersion(..)
, VersionError(..)
, HasVersionError(..)
, AsVersionError(..)
, versionMinorUnexpectedEof
, versionMajorUnexpectedEof
, getVersion
) where

import Data.Tickle(Get, word16be, (!-))
import Data.Word(Word16)
import Papa

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1 4.1. The ClassFile Structure>
data Version =
  Version {
    _version1 ::
      Word16
  , _version2 ::
      Word16
  }
  deriving (Eq, Ord, Show)

makeClassy ''Version

data VersionError =
  VersionMinorUnexpectedEof
  | VersionMajorUnexpectedEof
  deriving (Eq, Ord, Show)

makeClassy ''VersionError
makeClassyPrisms ''VersionError

versionMinorUnexpectedEof ::
  AsVersionError t =>
  t
versionMinorUnexpectedEof =
  _VersionMinorUnexpectedEof # ()

versionMajorUnexpectedEof ::
  AsVersionError t =>
  t
versionMajorUnexpectedEof =
  _VersionMajorUnexpectedEof # ()

getVersion ::
  AsVersionError e =>
  Get e Version
getVersion =
  do  mn <- versionMinorUnexpectedEof !- word16be
      mj <- versionMajorUnexpectedEof !- word16be
      return (Version mn mj)
