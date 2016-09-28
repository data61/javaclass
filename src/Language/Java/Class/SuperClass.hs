{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Java.Class.SuperClass(
  SuperClass(..)
, HasSuperClass(..)
, SuperClassError(..)
, HasSuperClassError(..)
, getSuperClass
) where

import Data.Tickle((!-), Get, word16be)
import Data.Word(Word16)
import Papa

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1 4.1. The ClassFile Structure>
newtype SuperClass =
  SuperClass Word16
  deriving (Eq, Ord, Show)

makeWrapped ''SuperClass
makeClassy ''SuperClass

data SuperClassError =
  SuperClassUnexpectedEof
  deriving (Eq, Ord, Show)

instance Wrapped SuperClassError where
  type Unwrapped SuperClassError = ()
  _Wrapped' =
    iso
      (\_ -> ())
      (\_ -> SuperClassUnexpectedEof)

instance Rewrapped SuperClassError SuperClassError

makeClassy ''SuperClassError

getSuperClass ::
  (Unwrapped e ~ (), Rewrapped e e) =>
  Get e SuperClass
getSuperClass =
  do  af <- _Wrapped # () !- word16be
      return (SuperClass af)
