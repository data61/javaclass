{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Java.Class.ThisAccessFlags(
  ThisAccessFlags(..)
, ThisAccessFlagsError(..)
, thisAccessFlagsUnexpectedEof
, getThisAccessFlags
) where

import Data.Tickle(Get, word16be, (!-))
import Data.Word(Word16)
import Papa

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1 4.1. The ClassFile Structure>
newtype ThisAccessFlags =
  ThisAccessFlags Word16
  deriving (Eq, Ord, Show)

makeWrapped ''ThisAccessFlags

data ThisAccessFlagsError =
  ThisAccessFlagsUnexpectedEof
  deriving (Eq, Ord, Show)

instance Wrapped ThisAccessFlagsError where
  type Unwrapped ThisAccessFlagsError = ()
  _Wrapped' =
    iso
      (\_ -> ())
      (\_ -> ThisAccessFlagsUnexpectedEof)

instance Rewrapped ThisAccessFlagsError ThisAccessFlagsError

thisAccessFlagsUnexpectedEof ::
  (Unwrapped t ~ (), Rewrapped t t) =>
  t
thisAccessFlagsUnexpectedEof =
  _Wrapped # ()

getThisAccessFlags ::
  (Unwrapped e ~ (), Rewrapped e e) =>
  Get e ThisAccessFlags
getThisAccessFlags =
  do  af <- thisAccessFlagsUnexpectedEof !- word16be
      return (ThisAccessFlags af)
