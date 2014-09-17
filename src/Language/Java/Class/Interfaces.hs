{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Java.Class.Interfaces(
  Interfaces(..)
, InterfacesError(..)
, AsInterfacesCountUnexpectedEof(..)
, interfacesCountUnexpectedEof
, AsInterfacesUnexpectedEof(..)
, interfaces
) where

import Control.Applicative(Applicative)
import Control.Lens(Optic', Choice, AsEmpty, Cons, prism', ( # ))
import Control.Monad(Monad(return))
import Control.Replicate(replicateO)
import Data.Eq(Eq)
import Data.Functor.Identity(Identity)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Data.Tagged(Tagged)
import Data.Tickle(Get, (!-), word16be)
import Data.Word(Word16)
import Prelude(Show)

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

data InterfacesError =
  InterfacesCountUnexpectedEof
  | InterfacesUnexpectedEof Word16
  deriving (Eq, Ord, Show)

class AsInterfacesCountUnexpectedEof p f s where
  _InterfacesCountUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsInterfacesCountUnexpectedEof p f InterfacesError where
  _InterfacesCountUnexpectedEof =
    prism'
      (\() -> InterfacesCountUnexpectedEof)
      (\e -> case e of
               InterfacesCountUnexpectedEof -> Just ()
               _ -> Nothing)
       
interfacesCountUnexpectedEof ::
  AsInterfacesCountUnexpectedEof Tagged Identity t =>
  t
interfacesCountUnexpectedEof =
  _InterfacesCountUnexpectedEof # ()

class AsInterfacesUnexpectedEof p f s where
  _InterfacesUnexpectedEof :: 
    Optic' p f s Word16

instance (Choice p, Applicative f) => AsInterfacesUnexpectedEof p f InterfacesError where
  _InterfacesUnexpectedEof =
   prism'
      InterfacesUnexpectedEof
      (\e -> case e of
               InterfacesUnexpectedEof w -> Just w
               _ -> Nothing)

interfaces ::
  (AsEmpty (i Word16), Cons (i Word16) (i Word16) Word16 Word16,
  AsInterfacesCountUnexpectedEof Tagged Identity e,
  AsInterfacesUnexpectedEof Tagged Identity e) =>
  Get e (Interfaces i)
interfaces =
  do c <- interfacesCountUnexpectedEof !- word16be
     i <- replicateO (\n -> _InterfacesUnexpectedEof # n !- word16be) c
     return (Interfaces c i)
