{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Language.Java.Class.Interfaces(
  Interfaces(..)
, HasInterfaces(..)
, InterfacesError(..)
, HasInterfacesError(..)
, AsInterfacesError(..)
, getInterfaces
) where

import Control.Replicate(replicateO)
import Data.Tickle(Get, (!-), word16be)
import Data.Word(Word16)
import Papa

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1 4.1. The ClassFile Structure>
data Interfaces i =
  Interfaces
    Word16
    (i Word16)

deriving instance Eq (i Word16) => Eq (Interfaces i)
deriving instance Ord (i Word16) => Ord (Interfaces i)
deriving instance Show (i Word16) => Show (Interfaces i)

makeClassy ''Interfaces

data InterfacesError =
  InterfacesCountUnexpectedEof
  | InterfacesUnexpectedEof Word16
  deriving (Eq, Ord, Show)

makeClassy ''InterfacesError
makeClassyPrisms ''InterfacesError

getInterfaces ::
  (AsInterfacesError e, Cons (i Word16) (i Word16) Word16 Word16, AsEmpty (i Word16)) =>
  Get e (Interfaces i)
getInterfaces =
  do c <- _InterfacesCountUnexpectedEof # () !- word16be
     i <- replicateO (\n -> _InterfacesUnexpectedEof # n !- word16be) c
     return (Interfaces c i)
