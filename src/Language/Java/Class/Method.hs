{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Java.Class.Method {- (
  Method(..)
, MethodErrorAttributeError(..)
, AsMethodErrorAttributeError(..)
, MethodError(..)
, AsMethodNameIndexUnexpectedEof(..)
, methodNameIndexUnexpectedEof
, AsMethodDescriptorIndexUnexpectedEof(..)
, methodDescriptorIndexUnexpectedEof
, AsMethodAttributeCountUnexpectedEof(..)
, methodAttributeCountUnexpectedEof
, AsMethodAttributeUnexpectedEof(..)
, AsMethodMethodAccessFlagsError(..)
, method
) -} where

import Control.Applicative(Applicative)
import Control.Category((.), id)
import Control.Lens(AsEmpty, Cons, Choice, Optic', Profunctor, prism', ( # ), _2, iso)
import Control.Monad(Monad(return))
import Control.Replicate(replicateO)
import Data.Eq(Eq)
import Data.Functor(Functor)
import Data.Functor.Identity(Identity)
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord)
import Data.Tagged(Tagged)
import Data.Tickle(Get, (!!-), (!-), word16be)
import Data.Word(Word8, Word16)
import Language.Java.Class.Attribute -- (AsAttributeNameIndexUnexpectedEof(_AttributeNameIndexUnexpectedEof), AsAttributeLengthUnexpectedEof(_AttributeLengthUnexpectedEof), AsAttributeUnexpectedEof(_AttributeUnexpectedEof), Attribute, AttributeError, attribute)
import Language.Java.Class.MethodAccessFlags -- (AsMethodAccessFlagsUnexpectedEof(_MethodAccessFlagsUnexpectedEof), MethodAccessFlags, MethodAccessFlagsError, methodAccessFlags)
import Prelude(Show)

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

newtype MethodErrorAttributeError =
  MethodErrorAttributeError
    AttributeError
  deriving (Eq, Ord, Show)

data MethodError =
  MethodNameIndexUnexpectedEof
  | MethodDescriptorIndexUnexpectedEof
  | MethodAttributeCountUnexpectedEof
  | MethodAttributeUnexpectedEof Word16 MethodErrorAttributeError
  | MethodMethodAccessFlagsError MethodAccessFlagsError
  deriving (Eq, Ord, Show)

{-

class AsMethodErrorAttributeError p f s where
  _MethodErrorAttributeError ::
    Optic' p f s MethodErrorAttributeError

instance AsMethodErrorAttributeError p f MethodErrorAttributeError where
  _MethodErrorAttributeError =
    id

instance (Profunctor p, Functor f) => AsMethodErrorAttributeError p f AttributeError where
  _MethodErrorAttributeError =
    iso
      MethodErrorAttributeError
      (\(MethodErrorAttributeError e) -> e)

instance AsAttributeUnexpectedEof p f MethodErrorAttributeError where
  _AttributeUnexpectedEof =
    _MethodErrorAttributeError . _AttributeUnexpectedEof

instance AsAttributeLengthUnexpectedEof p f MethodErrorAttributeError where  
  _AttributeLengthUnexpectedEof =
    _MethodErrorAttributeError . _AttributeLengthUnexpectedEof

instance AsAttributeNameIndexUnexpectedEof p f MethodErrorAttributeError where    
  _AttributeNameIndexUnexpectedEof =
    _MethodErrorAttributeError . _AttributeNameIndexUnexpectedEof

class AsMethodNameIndexUnexpectedEof p f s where
  _MethodNameIndexUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsMethodNameIndexUnexpectedEof p f MethodError where
  _MethodNameIndexUnexpectedEof =
    prism'
      (\() -> MethodNameIndexUnexpectedEof)
      (\e -> case e of
               MethodNameIndexUnexpectedEof -> Just ()
               _ -> Nothing)

methodNameIndexUnexpectedEof ::
  AsMethodNameIndexUnexpectedEof Tagged Identity t =>
  t
methodNameIndexUnexpectedEof=
  _MethodNameIndexUnexpectedEof # ()

class AsMethodDescriptorIndexUnexpectedEof p f s where
  _MethodDescriptorIndexUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsMethodDescriptorIndexUnexpectedEof p f MethodError where
  _MethodDescriptorIndexUnexpectedEof =
    prism'
      (\() -> MethodDescriptorIndexUnexpectedEof)
      (\e -> case e of
               MethodDescriptorIndexUnexpectedEof -> Just ()
               _ -> Nothing)

methodDescriptorIndexUnexpectedEof ::
  AsMethodDescriptorIndexUnexpectedEof Tagged Identity t =>
  t
methodDescriptorIndexUnexpectedEof=
  _MethodDescriptorIndexUnexpectedEof # ()

class AsMethodAttributeCountUnexpectedEof p f s where
  _MethodAttributeCountUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsMethodAttributeCountUnexpectedEof p f MethodError where
  _MethodAttributeCountUnexpectedEof =
    prism'
      (\() -> MethodAttributeCountUnexpectedEof)
      (\e -> case e of
               MethodAttributeCountUnexpectedEof -> Just ()
               _ -> Nothing)

methodAttributeCountUnexpectedEof ::
  AsMethodAttributeCountUnexpectedEof Tagged Identity t =>
  t
methodAttributeCountUnexpectedEof=
  _MethodAttributeCountUnexpectedEof # ()

class AsMethodAttributeUnexpectedEof p f s where
  _MethodAttributeUnexpectedEof :: 
    Optic' p f s (Word16, MethodErrorAttributeError)

instance (Choice p, Applicative f) => AsMethodAttributeUnexpectedEof p f MethodError where
  _MethodAttributeUnexpectedEof =
    prism'
      (\(w, r) -> MethodAttributeUnexpectedEof w r)
      (\e -> case e of
               MethodAttributeUnexpectedEof w r -> Just (w, r)
               _ -> Nothing)

instance (p ~ (->), Applicative f) => AsMethodErrorAttributeError p f MethodError where
  _MethodErrorAttributeError =
    _MethodAttributeUnexpectedEof . _2 . _MethodErrorAttributeError

instance (p ~ (->), Applicative f) => AsAttributeNameIndexUnexpectedEof p f MethodError where
  _AttributeNameIndexUnexpectedEof =
    _MethodAttributeUnexpectedEof . _2 . _AttributeNameIndexUnexpectedEof
    
instance (p ~ (->), Applicative f) => AsAttributeLengthUnexpectedEof p f MethodError where
  _AttributeLengthUnexpectedEof =
    _MethodAttributeUnexpectedEof . _2 . _AttributeLengthUnexpectedEof

instance (p ~ (->), Applicative f) => AsAttributeUnexpectedEof p f MethodError where
  _AttributeUnexpectedEof =
    _MethodAttributeUnexpectedEof . _2 . _AttributeUnexpectedEof

class AsMethodMethodAccessFlagsError p f s where
  _MethodMethodAccessFlagsError :: 
    Optic' p f s MethodAccessFlagsError

instance (Choice p, Applicative f) => AsMethodMethodAccessFlagsError p f MethodError where
  _MethodMethodAccessFlagsError =
    prism'
      MethodMethodAccessFlagsError
      (\e -> case e of
               MethodMethodAccessFlagsError r -> Just r
               _ -> Nothing)
      
instance (Choice p, Applicative f) => AsMethodAccessFlagsUnexpectedEof p f MethodError where
  _MethodAccessFlagsUnexpectedEof =
    _MethodMethodAccessFlagsError . _MethodAccessFlagsUnexpectedEof

method ::
  (AsEmpty (a Word8), AsEmpty (m (Attribute a1)),
  Cons (a Word8) (a Word8) Word8 Word8,
  Cons
    (m (Attribute a1)) (m (Attribute a1)) (Attribute a) (Attribute a),
  AsMethodMethodAccessFlagsError Tagged Identity e,
  AsMethodAttributeUnexpectedEof Tagged Identity e,
  AsMethodAttributeCountUnexpectedEof Tagged Identity e,
  AsMethodDescriptorIndexUnexpectedEof Tagged Identity e,
  AsMethodNameIndexUnexpectedEof Tagged Identity e) =>
  Get e (Method m a1)
method =
  do f <- (_MethodMethodAccessFlagsError #) !!- methodAccessFlags
     n <- methodNameIndexUnexpectedEof !- word16be
     d <- methodDescriptorIndexUnexpectedEof !- word16be
     c <- methodAttributeCountUnexpectedEof !- word16be
     a <- (_MethodAttributeUnexpectedEof #) !!- replicateO (\x -> ((,) x) !!- attribute) c
     return (Method f n d c a)
-}
