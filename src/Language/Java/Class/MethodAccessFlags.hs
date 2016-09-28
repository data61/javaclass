{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Java.Class.MethodAccessFlags(
  MethodAccessFlags(..)
, HasMethodAccessFlags(..)
, MethodAccessFlagsError(..)
, HasMethodAccessFlagsError(..)
, getMethodAccessFlags
) where

import Data.Tickle(Get, (!-), word16be)
import Data.Word(Word16)
import Papa

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.6 4.6. Methods>
newtype MethodAccessFlags =
  MethodAccessFlags
    Word16
  deriving (Eq, Ord, Show)

makeWrapped ''MethodAccessFlags
makeClassy ''MethodAccessFlags

data MethodAccessFlagsError =
  MethodAccessFlagsUnexpectedEof
  deriving (Eq, Ord, Show)

instance Wrapped MethodAccessFlagsError where
  type Unwrapped MethodAccessFlagsError = ()
  _Wrapped' =
    iso
      (\_ -> ())
      (\_ -> MethodAccessFlagsUnexpectedEof)

instance Rewrapped MethodAccessFlagsError MethodAccessFlagsError

makeClassy ''MethodAccessFlagsError

getMethodAccessFlags ::
  (Unwrapped e ~ (), Rewrapped e e) =>
  Get e MethodAccessFlags
getMethodAccessFlags =
  do  af <- _Wrapped # () !- word16be
      return (MethodAccessFlags af)
