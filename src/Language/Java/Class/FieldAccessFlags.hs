{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Java.Class.FieldAccessFlags(
  FieldAccessFlags(..)
, FieldAccessFlagsError(..)
, fieldAccessFlagsUnexpectedEof
, getFieldAccessFlags
) where

import Data.Tickle(Get, (!-), word16be)
import Data.Word(Word16)
import Papa

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.5 4.5. Fields>
newtype FieldAccessFlags =
  FieldAccessFlags
    Word16
  deriving (Eq, Ord, Show)

makeWrapped ''FieldAccessFlags

data FieldAccessFlagsError =
  FieldAccessFlagsUnexpectedEof
  deriving (Eq, Ord, Show)

instance Wrapped FieldAccessFlagsError where
  type Unwrapped FieldAccessFlagsError = ()
  _Wrapped' =
    iso
      (\_ -> ())
      (\_ -> FieldAccessFlagsUnexpectedEof)

instance Rewrapped FieldAccessFlagsError FieldAccessFlagsError

fieldAccessFlagsUnexpectedEof ::
  (Unwrapped t ~ (), Rewrapped t t) =>
  t
fieldAccessFlagsUnexpectedEof =
  _Wrapped # ()

getFieldAccessFlags ::
  (Unwrapped e ~ (), Rewrapped e e) =>
  Get e FieldAccessFlags
getFieldAccessFlags =
  do  af <- fieldAccessFlagsUnexpectedEof !- word16be
      return (FieldAccessFlags af)  
