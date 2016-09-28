{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Java.Class.ThisClass(
  ThisClass(..)
, HasThisClass(..)
, ThisClassError(..)
, HasThisClassError(..)
, getThisClass
) where

import Data.Tickle(Get, word16be, (!-))
import Data.Word(Word16)
import Papa

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1 4.1. The ClassFile Structure>
newtype ThisClass =
  ThisClass Word16
  deriving (Eq, Ord, Show)

makeWrapped ''ThisClass
makeClassy ''ThisClass

data ThisClassError =
  ThisClassUnexpectedEof
  deriving (Eq, Ord, Show)

instance Wrapped ThisClassError where
  type Unwrapped ThisClassError = ()
  _Wrapped' =
    iso
      (\_ -> ())
      (\_ -> ThisClassUnexpectedEof)

instance Rewrapped ThisClassError ThisClassError

makeClassy ''ThisClassError

getThisClass ::
  (Unwrapped e ~ (), Rewrapped e e) =>
  Get e ThisClass
getThisClass =
  do  af <- _Wrapped # () !- word16be
      return (ThisClass af)
