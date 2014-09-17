{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Java.Class.Methods(
  Methods(..)
, MethodsError(..)
, AsMethodsInfoUnexpectedEof(..)
, methodsInfoUnexpectedEof
, AsMethodsMethodError(..)
, methods
) where

import Control.Applicative(Applicative)
import Control.Category((.))
import Control.Lens(AsEmpty, Cons, Optic', Choice, prism', ( # ), _2)
import Control.Monad(Monad(return))
import Control.Replicate(replicateO)
import Data.Eq(Eq)
import Data.Functor.Identity(Identity)
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord)
import Data.Tagged(Tagged)
import Data.Tickle(Get, (!!-), (!-), word16be)
import Data.Word(Word8, Word16)
import Language.Java.Class.Attribute(AsAttributeNameIndexUnexpectedEof(_AttributeNameIndexUnexpectedEof), AsAttributeLengthUnexpectedEof(_AttributeLengthUnexpectedEof), AsAttributeUnexpectedEof(_AttributeUnexpectedEof), Attribute)
import Language.Java.Class.Method(AsMethodErrorAttributeError(_MethodErrorAttributeError), AsMethodNameIndexUnexpectedEof(_MethodNameIndexUnexpectedEof), AsMethodDescriptorIndexUnexpectedEof(_MethodDescriptorIndexUnexpectedEof), AsMethodAttributeCountUnexpectedEof(_MethodAttributeCountUnexpectedEof), AsMethodAttributeUnexpectedEof(_MethodAttributeUnexpectedEof), AsMethodMethodAccessFlagsError(_MethodMethodAccessFlagsError), Method, MethodError, method)
import Language.Java.Class.MethodAccessFlags(AsMethodAccessFlagsUnexpectedEof(_MethodAccessFlagsUnexpectedEof))
import Prelude(Show)

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1 4.1. The ClassFile Structure>
data Methods m a s =
  Methods
    Word16
    (s (Method m a))

deriving instance Eq (s (Method m a)) => Eq (Methods m a s)
deriving instance Ord (s (Method m a)) => Ord (Methods m a s)
deriving instance Show (s (Method m a)) => Show (Methods m a s)

data MethodsError =
  MethodsInfoUnexpectedEof
  | MethodsMethodError Word16 MethodError
  deriving (Eq, Ord, Show)

class AsMethodsInfoUnexpectedEof p f s where
  _MethodsInfoUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsMethodsInfoUnexpectedEof p f MethodsError where
  _MethodsInfoUnexpectedEof =
    prism'
      (\() -> MethodsInfoUnexpectedEof)
      (\e -> case e of
               MethodsInfoUnexpectedEof -> Just ()
               _ -> Nothing)

methodsInfoUnexpectedEof ::
  AsMethodsInfoUnexpectedEof Tagged Identity t =>
  t
methodsInfoUnexpectedEof=
  _MethodsInfoUnexpectedEof # ()

class AsMethodsMethodError p f s where
  _MethodsMethodError :: 
    Optic' p f s (Word16, MethodError)

instance (Choice p, Applicative f) => AsMethodsMethodError p f MethodsError where
  _MethodsMethodError =
    prism'
      (\(w, r) -> MethodsMethodError w r)
      (\e -> case e of
               MethodsMethodError w r -> Just (w, r)
               _ -> Nothing)

instance (p ~ (->), Applicative f) => AsMethodNameIndexUnexpectedEof p f MethodsError where
  _MethodNameIndexUnexpectedEof =
    _MethodsMethodError . _2 . _MethodNameIndexUnexpectedEof

instance (p ~ (->), Applicative f) => AsMethodDescriptorIndexUnexpectedEof p f MethodsError where
  _MethodDescriptorIndexUnexpectedEof =
    _MethodsMethodError . _2 . _MethodDescriptorIndexUnexpectedEof

instance (p ~ (->), Applicative f) => AsMethodAttributeCountUnexpectedEof p f MethodsError where
  _MethodAttributeCountUnexpectedEof =
    _MethodsMethodError . _2 . _MethodAttributeCountUnexpectedEof

instance (p ~ (->), Applicative f) => AsMethodAttributeUnexpectedEof p f MethodsError where
  _MethodAttributeUnexpectedEof =
    _MethodsMethodError . _2 . _MethodAttributeUnexpectedEof

instance (p ~ (->), Applicative f) => AsAttributeNameIndexUnexpectedEof p f MethodsError where
  _AttributeNameIndexUnexpectedEof =
    _MethodsMethodError . _2 . _AttributeNameIndexUnexpectedEof

instance (p ~ (->), Applicative f) => AsAttributeLengthUnexpectedEof p f MethodsError where
  _AttributeLengthUnexpectedEof =
    _MethodsMethodError . _2 . _AttributeLengthUnexpectedEof

instance (p ~ (->), Applicative f) => AsMethodErrorAttributeError p f MethodsError where
  _MethodErrorAttributeError =
    _MethodsMethodError . _2 . _MethodErrorAttributeError

instance (p ~ (->), Applicative f) => AsAttributeUnexpectedEof p f MethodsError where
  _AttributeUnexpectedEof =
    _MethodsMethodError . _2 . _AttributeUnexpectedEof

instance (p ~ (->), Applicative f) => AsMethodMethodAccessFlagsError p f MethodsError where
  _MethodMethodAccessFlagsError =
    _MethodsMethodError . _2 . _MethodMethodAccessFlagsError

instance (p ~ (->), Applicative f) => AsMethodAccessFlagsUnexpectedEof p f MethodsError where
  _MethodAccessFlagsUnexpectedEof =
    _MethodsMethodError . _2 . _MethodAccessFlagsUnexpectedEof

methods ::
  (AsEmpty (m (Attribute a1)), AsEmpty (a Word8),
    AsEmpty (s (Method m1 a2)),
    Cons
      (m (Attribute a1)) (m (Attribute a1)) (Attribute a) (Attribute a),
    Cons (a Word8) (a Word8) Word8 Word8,
    Cons
      (s (Method m1 a2)) (s (Method m1 a2)) (Method m a1) (Method m a1),
    AsMethodsInfoUnexpectedEof Tagged Identity e,
    AsMethodsMethodError Tagged Identity e) =>
  Get e (Methods m1 a2 s)
methods =
  do c <- methodsInfoUnexpectedEof !- word16be
     i <- (_MethodsMethodError #) !!- replicateO (\n -> ((,) n) !!- method) c
     return (Methods c i)
