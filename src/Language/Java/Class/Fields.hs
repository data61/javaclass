{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Language.Java.Class.Fields(
  Fields(..)
, HasFields(..)
, FieldsError(..)
, HasFieldsError(..)
, AsFieldsError(..)
, getFields
) where

import Control.Replicate(replicateO)
import Data.Tickle(Get, (!!-), (!-), word16be)
import Data.Word(Word8, Word16)
import Language.Java.Class.Attribute(Attribute)
import Language.Java.Class.Field(Field, FieldError, getField)
import Papa

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1 4.1. The ClassFile Structure>
data Fields a f s =
  Fields
    Word16
    (s (Field a f))

makeClassy ''Fields

deriving instance Eq (s (Field a f)) => Eq (Fields a f s)
deriving instance Ord (s (Field a f)) => Ord (Fields a f s)
deriving instance Show (s (Field a f)) => Show (Fields a f s)

data FieldsError =
  FieldsFieldInfoUnexpectedEof
  | FieldsFieldError Word16 FieldError
  deriving (Eq, Ord, Show)

makeClassy ''FieldsError
makeClassyPrisms ''FieldsError

getFields ::
  (AsFieldsError e, Cons (s (Field a2 f1)) (s (Field a2 f1)) (Field a1 f) (Field a1 f), Cons (a Word8) (a Word8) Word8 Word8, Cons (f (Attribute a1)) (f (Attribute a1)) (Attribute a) (Attribute a), AsEmpty (s (Field a2 f1)), AsEmpty (a Word8), AsEmpty (f (Attribute a1))) =>
  Get e (Fields a2 f1 s)
getFields =
  do  c <- (_FieldsFieldInfoUnexpectedEof # ()) !- word16be
      i <- (_FieldsFieldError #) !!- replicateO (\n -> ((,) n) !!- getField) c
      return (Fields c i)
