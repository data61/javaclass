{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Language.Java.Class.Field(
  Field(..)
, HasField(..)
, FieldErrorAttributeError(..)
, FieldError(..)
, HasFieldError(..)
, AsFieldError(..)
, fieldFieldAccessFlagsError
, fieldNameIndexUnexpectedEof
, fieldDescriptorIndexUnexpectedEof
, fieldAttributeCountUnexpectedEof
, fieldAttributeError
, getField
) where

import Control.Replicate(replicateO)
import Data.Tickle(Get, (!!-), (!-), word16be)
import Data.Word(Word8, Word16)
import Language.Java.Class.Attribute(Attribute, AttributeError, AsAttributeError(_AttributeError), getAttribute)
import Language.Java.Class.FieldAccessFlags(FieldAccessFlags, FieldAccessFlagsError, getFieldAccessFlags)
import Papa

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.5 4.5. Fields>
data Field a f =
  Field {
    _access_flags ::
      FieldAccessFlags
  , _name_index ::
      Word16
  , _descriptor_index ::
      Word16
  , _field_attribute_count ::
      Word16
  , _field_attrs ::
      f (Attribute a)
  }

deriving instance Eq (f (Attribute a)) => Eq (Field a f)
deriving instance Ord (f (Attribute a)) => Ord (Field a f)
deriving instance Show (f (Attribute a)) => Show (Field a f)

makeClassy ''Field

newtype FieldErrorAttributeError =
  FieldErrorAttributeError
    AttributeError
  deriving (Eq, Ord, Show)

makeWrapped ''FieldErrorAttributeError

instance AsAttributeError FieldErrorAttributeError where
  _AttributeError =
    _Wrapped

data FieldError =
  FieldFieldAccessFlagsError FieldAccessFlagsError 
  | FieldNameIndexUnexpectedEof
  | FieldDescriptorIndexUnexpectedEof
  | FieldAttributeCountUnexpectedEof
  | FieldAttributeError Word16 FieldErrorAttributeError
  deriving (Eq, Ord, Show)

makeClassy ''FieldError
makeClassyPrisms ''FieldError

fieldFieldAccessFlagsError ::
  AsFieldError t =>
  FieldAccessFlagsError
  -> t
fieldFieldAccessFlagsError e =
  _FieldFieldAccessFlagsError # e

fieldNameIndexUnexpectedEof ::
  AsFieldError t =>
  t
fieldNameIndexUnexpectedEof =
  _FieldNameIndexUnexpectedEof # ()
        
fieldDescriptorIndexUnexpectedEof ::
  AsFieldError t =>
  t
fieldDescriptorIndexUnexpectedEof =
  _FieldDescriptorIndexUnexpectedEof # ()

fieldAttributeCountUnexpectedEof ::
  AsFieldError t =>
  t
fieldAttributeCountUnexpectedEof =
  _FieldAttributeCountUnexpectedEof # ()
        
fieldAttributeError ::
  AsFieldError t =>
  Word16
  -> FieldErrorAttributeError
  -> t
fieldAttributeError w e =
  _FieldAttributeError # (w, e)

getField ::
  (AsFieldError e, Cons (f (Attribute a1)) (f (Attribute a1)) (Attribute a) (Attribute a), Cons (a Word8) (a Word8) Word8 Word8, AsEmpty (f (Attribute a1)), AsEmpty (a Word8)) =>
  Get e (Field a1 f)
getField =
  do  f <- (_FieldFieldAccessFlagsError #) !!- getFieldAccessFlags
      n <- fieldNameIndexUnexpectedEof !- word16be
      d <- fieldDescriptorIndexUnexpectedEof !- word16be
      c <- fieldAttributeCountUnexpectedEof !- word16be
      a <- replicateO (\x -> (\w -> _FieldAttributeError # (x, w)) !!- getAttribute) c
      return (Field f n d c a)
