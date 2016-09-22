{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Java.Class.Fields {- (
  Fields(..)
, FieldsError(..)
, AsFieldsFieldInfoUnexpectedEof(..)
, fieldsFieldInfoUnexpectedEof
, AsFieldsFieldError(..)
, fields
) -} where

import Control.Applicative(Applicative)
import Control.Category((.))
import Control.Lens(Cons, AsEmpty, Optic', Choice, prism', ( # ), _2)
import Control.Monad(Monad(return))
import Control.Replicate(replicateO)
import Data.Eq(Eq)
import Data.Functor.Identity(Identity)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Data.Tagged(Tagged)
import Data.Tickle(Get, (!!-), (!-), word16be)
import Data.Word(Word8, Word16)
import Language.Java.Class.Attribute -- (AsAttributeNameIndexUnexpectedEof(_AttributeNameIndexUnexpectedEof), AsAttributeLengthUnexpectedEof(_AttributeLengthUnexpectedEof), AsAttributeUnexpectedEof(_AttributeUnexpectedEof), Attribute)
import Language.Java.Class.Field -- (AsFieldErrorAttributeError(_FieldErrorAttributeError), AsFieldFieldAccessFlagsError(_FieldFieldAccessFlagsError), AsFieldNameIndexUnexpectedEof(_FieldNameIndexUnexpectedEof), AsFieldDescriptorIndexUnexpectedEof(_FieldDescriptorIndexUnexpectedEof), AsFieldAttributeCountUnexpectedEof(_FieldAttributeCountUnexpectedEof), AsFieldAttributeError(_FieldAttributeError), Field, FieldError, field)
import Prelude(Show)

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1 4.1. The ClassFile Structure>
data Fields a f s =
  Fields
    Word16
    (s (Field a f))

deriving instance Eq (s (Field a f)) => Eq (Fields a f s)
deriving instance Ord (s (Field a f)) => Ord (Fields a f s)
deriving instance Show (s (Field a f)) => Show (Fields a f s)

data FieldsError =
  FieldsFieldInfoUnexpectedEof
  | FieldsFieldError Word16 FieldError
  deriving (Eq, Ord, Show)

{-
class AsFieldsFieldInfoUnexpectedEof p f s where
  _FieldsFieldInfoUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsFieldsFieldInfoUnexpectedEof p f FieldsError where
  _FieldsFieldInfoUnexpectedEof =
    prism'
      (\() -> FieldsFieldInfoUnexpectedEof)
      (\e -> case e of
               FieldsFieldInfoUnexpectedEof -> Just ()
               _ -> Nothing)

fieldsFieldInfoUnexpectedEof ::
  AsFieldsFieldInfoUnexpectedEof Tagged Identity t =>
  t
fieldsFieldInfoUnexpectedEof=
  _FieldsFieldInfoUnexpectedEof # ()

class AsFieldsFieldError p f s where
  _FieldsFieldError :: 
    Optic' p f s (Word16, FieldError)

instance (Choice p, Applicative f) => AsFieldsFieldError p f FieldsError where
  _FieldsFieldError =
   prism'
      (\(w, r) -> FieldsFieldError w r)
      (\e -> case e of
               FieldsFieldError w r -> Just (w, r)
               _ -> Nothing)

instance (p ~ (->), Applicative f) => AsFieldFieldAccessFlagsError p f FieldsError where
  _FieldFieldAccessFlagsError =
    _FieldsFieldError . _2 . _FieldFieldAccessFlagsError

instance (p ~ (->), Applicative f) => AsFieldNameIndexUnexpectedEof p f FieldsError where
  _FieldNameIndexUnexpectedEof =
    _FieldsFieldError . _2 . _FieldNameIndexUnexpectedEof

instance (p ~ (->), Applicative f) => AsFieldDescriptorIndexUnexpectedEof p f FieldsError where
  _FieldDescriptorIndexUnexpectedEof =
    _FieldsFieldError . _2 . _FieldDescriptorIndexUnexpectedEof

instance (p ~ (->), Applicative f) => AsFieldAttributeCountUnexpectedEof p f FieldsError where
  _FieldAttributeCountUnexpectedEof =
    _FieldsFieldError . _2 . _FieldAttributeCountUnexpectedEof

instance (p ~ (->), Applicative f) => AsFieldAttributeError p f FieldsError where
  _FieldAttributeError =
    _FieldsFieldError . _2 . _FieldAttributeError

instance (p ~ (->), Applicative f) => AsFieldErrorAttributeError p f FieldsError where
  _FieldErrorAttributeError =
    _FieldsFieldError . _2 . _FieldErrorAttributeError

instance (p ~ (->), Applicative f) => AsAttributeNameIndexUnexpectedEof p f FieldsError where
  _AttributeNameIndexUnexpectedEof =
    _FieldsFieldError . _2 . _AttributeNameIndexUnexpectedEof

instance (p ~ (->), Applicative f) => AsAttributeLengthUnexpectedEof p f FieldsError where
  _AttributeLengthUnexpectedEof =
    _FieldsFieldError . _2 . _AttributeLengthUnexpectedEof

instance (p ~ (->), Applicative f) => AsAttributeUnexpectedEof p f FieldsError where
  _AttributeUnexpectedEof =
    _FieldsFieldError . _2 . _AttributeUnexpectedEof

fields ::
  (AsEmpty (f (Attribute a1)), AsEmpty (a Word8),
    AsEmpty (s (Field a2 f1)),
    Cons
      (f (Attribute a1)) (f (Attribute a1)) (Attribute a) (Attribute a),
    Cons (a Word8) (a Word8) Word8 Word8,
    Cons (s (Field a2 f1)) (s (Field a2 f1)) (Field a1 f) (Field a1 f),
    AsFieldsFieldInfoUnexpectedEof Tagged Identity e,
    AsFieldsFieldError Tagged Identity e) =>
  Get e (Fields a2 f1 s)
fields =
  do c <- fieldsFieldInfoUnexpectedEof !- word16be
     i <- (_FieldsFieldError #) !!- replicateO (\n -> ((,) n) !!- field) c
     return (Fields c i)
-}
