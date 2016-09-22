{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Java.Class.Field {- (
  Field(..)
, FieldErrorAttributeError(..)
, AsFieldErrorAttributeError(..)
, FieldError(..)
, AsFieldFieldAccessFlagsError(..)
, AsFieldNameIndexUnexpectedEof(..)
, fieldNameIndexUnexpectedEof
, AsFieldDescriptorIndexUnexpectedEof(..)
, fieldDescriptorIndexUnexpectedEof
, AsFieldAttributeCountUnexpectedEof(..)
, fieldAttributeCountUnexpectedEof
, AsFieldAttributeError(..)
, field
) -} where

import Control.Applicative(Applicative)
import Control.Category((.), id)
import Control.Lens(AsEmpty, Cons, Optic', Choice, Profunctor, prism', ( # ), _2, iso)
import Control.Monad(Monad(return))
import Control.Replicate(replicateO)
import Data.Eq(Eq)
import Data.Functor(Functor)
import Data.Functor.Identity(Identity)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Data.Tagged(Tagged)
import Data.Tickle(Get, (!!-), (!-), word16be)
import Data.Word(Word8, Word16)
import Language.Java.Class.Attribute -- (AsAttributeNameIndexUnexpectedEof(_AttributeNameIndexUnexpectedEof), AsAttributeLengthUnexpectedEof(_AttributeLengthUnexpectedEof), AsAttributeUnexpectedEof(_AttributeUnexpectedEof), Attribute, AttributeError, attribute)
import Language.Java.Class.FieldAccessFlags
import Prelude(Show)

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.5 4.5. Fields>
data Field a f =
  Field
    FieldAccessFlags
    Word16 -- name_index
    Word16 -- descriptor_index
    Word16 -- attribute_count
    (f (Attribute a))

deriving instance Eq (f (Attribute a)) => Eq (Field a f)
deriving instance Ord (f (Attribute a)) => Ord (Field a f)
deriving instance Show (f (Attribute a)) => Show (Field a f)

newtype FieldErrorAttributeError =
  FieldErrorAttributeError
    AttributeError
  deriving (Eq, Ord, Show)


data FieldError =
  FieldFieldAccessFlagsError FieldAccessFlagsError 
  | FieldNameIndexUnexpectedEof
  | FieldDescriptorIndexUnexpectedEof
  | FieldAttributeCountUnexpectedEof
  | FieldAttributeError Word16 FieldErrorAttributeError
  deriving (Eq, Ord, Show)

{-
class AsFieldErrorAttributeError p f s where
  _FieldErrorAttributeError ::
    Optic' p f s FieldErrorAttributeError

instance AsFieldErrorAttributeError p f FieldErrorAttributeError where
  _FieldErrorAttributeError =
    id

instance (Profunctor p, Functor f) => AsFieldErrorAttributeError p f AttributeError where
  _FieldErrorAttributeError =
    iso
      FieldErrorAttributeError
      (\(FieldErrorAttributeError e) -> e)

instance AsAttributeUnexpectedEof p f FieldErrorAttributeError where
  _AttributeUnexpectedEof =
    _FieldErrorAttributeError . _AttributeUnexpectedEof

instance AsAttributeLengthUnexpectedEof p f FieldErrorAttributeError where  
  _AttributeLengthUnexpectedEof =
    _FieldErrorAttributeError . _AttributeLengthUnexpectedEof

instance AsAttributeNameIndexUnexpectedEof p f FieldErrorAttributeError where    
  _AttributeNameIndexUnexpectedEof =
    _FieldErrorAttributeError . _AttributeNameIndexUnexpectedEof

class AsFieldFieldAccessFlagsError p f s where
  _FieldFieldAccessFlagsError :: 
    Optic' p f s FieldAccessFlagsError

instance (Choice p, Applicative f) => AsFieldFieldAccessFlagsError p f FieldError where
  _FieldFieldAccessFlagsError =
    prism'
      FieldFieldAccessFlagsError
      (\e -> case e of
               FieldFieldAccessFlagsError r -> Just r
               _ -> Nothing)
       
class AsFieldNameIndexUnexpectedEof p f s where
  _FieldNameIndexUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsFieldNameIndexUnexpectedEof p f FieldError where
  _FieldNameIndexUnexpectedEof =
    prism'
      (\() -> FieldNameIndexUnexpectedEof)
      (\e -> case e of
               FieldNameIndexUnexpectedEof -> Just ()
               _ -> Nothing)   
       
fieldNameIndexUnexpectedEof ::
  AsFieldNameIndexUnexpectedEof Tagged Identity t =>
  t
fieldNameIndexUnexpectedEof =
  _FieldNameIndexUnexpectedEof # ()
        
class AsFieldDescriptorIndexUnexpectedEof p f s where
  _FieldDescriptorIndexUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsFieldDescriptorIndexUnexpectedEof p f FieldError where
  _FieldDescriptorIndexUnexpectedEof =
    prism'
      (\() -> FieldDescriptorIndexUnexpectedEof)
      (\e -> case e of
               FieldDescriptorIndexUnexpectedEof -> Just ()
               _ -> Nothing)   
    
fieldDescriptorIndexUnexpectedEof ::
  AsFieldDescriptorIndexUnexpectedEof Tagged Identity t =>
  t
fieldDescriptorIndexUnexpectedEof =
  _FieldDescriptorIndexUnexpectedEof # ()
        
class AsFieldAttributeCountUnexpectedEof p f s where
  _FieldAttributeCountUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsFieldAttributeCountUnexpectedEof p f FieldError where
  _FieldAttributeCountUnexpectedEof =
    prism'
      (\() -> FieldAttributeCountUnexpectedEof)
      (\e -> case e of
               FieldAttributeCountUnexpectedEof -> Just ()
               _ -> Nothing)   

fieldAttributeCountUnexpectedEof ::
  AsFieldAttributeCountUnexpectedEof Tagged Identity t =>
  t
fieldAttributeCountUnexpectedEof =
  _FieldAttributeCountUnexpectedEof # ()
        
class AsFieldAttributeError p f s where
  _FieldAttributeError :: 
    Optic' p f s (Word16, FieldErrorAttributeError)

instance (Choice p, Applicative f) => AsFieldAttributeError p f FieldError where
  _FieldAttributeError =
    prism'
      (\(w, r) -> FieldAttributeError w r)
      (\e -> case e of
               FieldAttributeError w r -> Just (w, r)
               _ -> Nothing)

instance (p ~ (->), Applicative f) => AsFieldErrorAttributeError p f FieldError where
  _FieldErrorAttributeError =
    _FieldAttributeError . _2 . _FieldErrorAttributeError

instance (p ~ (->), Applicative f) => AsAttributeNameIndexUnexpectedEof p f FieldError where
  _AttributeNameIndexUnexpectedEof =
    _FieldAttributeError . _2 . _AttributeNameIndexUnexpectedEof

instance (p ~ (->), Applicative f) => AsAttributeLengthUnexpectedEof p f FieldError where
  _AttributeLengthUnexpectedEof =
    _FieldAttributeError . _2 . _AttributeLengthUnexpectedEof

instance (p ~ (->), Applicative f) => AsAttributeUnexpectedEof p f FieldError where
  _AttributeUnexpectedEof =
    _FieldAttributeError . _2 . _AttributeUnexpectedEof

field ::
  (AsEmpty (a Word8), AsEmpty (f (Attribute a1)),
    Cons (a Word8) (a Word8) Word8 Word8,
    Cons
      (f (Attribute a1)) (f (Attribute a1)) (Attribute a) (Attribute a),
    AsFieldFieldAccessFlagsError Tagged Identity e,
    AsFieldNameIndexUnexpectedEof Tagged Identity e,
    AsFieldDescriptorIndexUnexpectedEof Tagged Identity e,
    AsFieldAttributeCountUnexpectedEof Tagged Identity e,
    AsFieldAttributeError Tagged Identity e) =>
  Get e (Field a1 f)
field =
  do f <- (_FieldFieldAccessFlagsError #) !!- fieldAccessFlags
     n <- fieldNameIndexUnexpectedEof !- word16be
     d <- fieldDescriptorIndexUnexpectedEof !- word16be
     c <- fieldAttributeCountUnexpectedEof !- word16be
     a <- replicateO (\x -> (\w -> _FieldAttributeError # (x, w)) !!- attribute) c
     return (Field f n d c a)
-}
