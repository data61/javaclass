{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Java.Class.Method(
  Method(..)
, HasMethod(..)
, MethodErrorAttributeError(..)
, HasMethodErrorAttributeError(..)
, MethodError(..)
, HasMethodError(..)
, AsMethodError(..)
, getMethod
) where

import Control.Replicate(replicateO)
import Data.Tickle(Get, (!!-), (!-), word16be)
import Data.Word(Word8, Word16)
import Language.Java.Class.Attribute(Attribute, AttributeError, AsAttributeError(_AttributeError), getAttribute)
import Language.Java.Class.MethodAccessFlags(MethodAccessFlags, MethodAccessFlagsError, getMethodAccessFlags)
import Papa

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.6 4.6. Methods>
data Method m a =
  Method
    MethodAccessFlags
    Word16 -- name_index
    Word16 -- descriptor_index
    Word16 -- attribute_count
    (m (Attribute a))
  
deriving instance Eq (m (Attribute a)) => Eq (Method m a)
deriving instance Ord (m (Attribute a)) => Ord (Method m a)
deriving instance Show (m (Attribute a)) => Show (Method m a)

makeClassy ''Method

newtype MethodErrorAttributeError =
  MethodErrorAttributeError
    AttributeError
  deriving (Eq, Ord, Show)

makeWrapped ''MethodErrorAttributeError
makeClassy ''MethodErrorAttributeError

instance AsAttributeError MethodErrorAttributeError where
  _AttributeError =
    _Wrapped . _AttributeError
    
data MethodError =
  MethodNameIndexUnexpectedEof
  | MethodDescriptorIndexUnexpectedEof
  | MethodAttributeCountUnexpectedEof
  | MethodAttributeUnexpectedEof Word16 MethodErrorAttributeError
  | MethodMethodAccessFlagsError MethodAccessFlagsError
  deriving (Eq, Ord, Show)

makeClassy ''MethodError
makeClassyPrisms ''MethodError

getMethod ::
  (AsMethodError e, Cons (m (Attribute a1)) (m (Attribute a1)) (Attribute a) (Attribute a), Cons (a Word8) (a Word8) Word8 Word8, AsEmpty (m (Attribute a1)), AsEmpty (a Word8)) =>
  Get e (Method m a1)
getMethod =
  do  f <- (_MethodMethodAccessFlagsError #) !!- getMethodAccessFlags
      n <- (_MethodNameIndexUnexpectedEof # ()) !- word16be
      d <- (_MethodDescriptorIndexUnexpectedEof # ()) !- word16be
      c <- (_MethodAttributeCountUnexpectedEof # ()) !- word16be
      a <- (_MethodAttributeUnexpectedEof #) !!- replicateO (\x -> ((,) x) !!- getAttribute) c
      return (Method f n d c a)
