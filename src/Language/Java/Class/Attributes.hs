{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Java.Class.Attributes {- (
  Attributes(..)
, AttributesError(..)
, AsAttributesInfoUnexpectedEof(..)
, attributesInfoUnexpectedEof
, AsAttributesAttributeError(..)
, attributes
) -} where

import Control.Applicative(Applicative)
import Control.Category((.))
import Control.Lens
import Control.Monad(Monad(return))
import Control.Replicate(replicateO)
import Data.Eq(Eq)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Data.Tagged(Tagged)
import Data.Tickle(Get, word16be, (!!-), (!-))
import Data.Word(Word8, Word16)
import Language.Java.Class.Attribute -- (AsAttributeNameIndexUnexpectedEof(_AttributeNameIndexUnexpectedEof), AsAttributeLengthUnexpectedEof(_AttributeLengthUnexpectedEof), AsAttributeUnexpectedEof(_AttributeUnexpectedEof), Attribute, AttributeError, attribute)
import Prelude(Show)

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1 4.1. The ClassFile Structure>
data Attributes a s =
  Attributes
    Word16
    (s (Attribute a))

deriving instance Eq (s (Attribute a)) => Eq (Attributes a s)
deriving instance Ord (s (Attribute a)) => Ord (Attributes a s)
deriving instance Show (s (Attribute a)) => Show (Attributes a s)

data AttributesError =
  AttributesInfoUnexpectedEof
  | AttributesAttributeError Word16 AttributeError
  deriving (Eq, Ord, Show)

{-
class AsAttributesInfoUnexpectedEof p f s where
  _AttributesInfoUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsAttributesInfoUnexpectedEof p f AttributesError where
  _AttributesInfoUnexpectedEof =
    prism'
      (\() -> AttributesInfoUnexpectedEof)
      (\e -> case e of
               AttributesInfoUnexpectedEof -> Just ()
               _ -> Nothing)

attributesInfoUnexpectedEof ::
  AsAttributesInfoUnexpectedEof Tagged Identity t =>
  t
attributesInfoUnexpectedEof=
  _AttributesInfoUnexpectedEof # ()

class AsAttributesAttributeError p f s where
  _AttributesAttributeError :: 
    Optic' p f s (Word16, AttributeError)

instance (Choice p, Applicative f) => AsAttributesAttributeError p f AttributesError where
  _AttributesAttributeError =
    prism'
      (\(w, r) -> AttributesAttributeError w r)
      (\e -> case e of
               AttributesAttributeError w r -> Just (w, r)
               _ -> Nothing)

instance (p ~ (->), Applicative f) => AsAttributeNameIndexUnexpectedEof p f AttributesError where
  _AttributeNameIndexUnexpectedEof =
    _AttributesAttributeError . _2 . _AttributeNameIndexUnexpectedEof

instance (p ~ (->), Applicative f) => AsAttributeLengthUnexpectedEof p f AttributesError where
  _AttributeLengthUnexpectedEof =
    _AttributesAttributeError . _2 . _AttributeLengthUnexpectedEof

instance (p ~ (->), Applicative f) => AsAttributeUnexpectedEof p f AttributesError where
  _AttributeUnexpectedEof =
    _AttributesAttributeError . _2 . _AttributeUnexpectedEof

attributes ::
  (AsEmpty (a Word8), AsEmpty (s (Attribute a1)),
    Cons (a Word8) (a Word8) Word8 Word8,
    Cons
      (s (Attribute a1)) (s (Attribute a1)) (Attribute a) (Attribute a),
    AsAttributesInfoUnexpectedEof Tagged Identity e,
    AsAttributesAttributeError Tagged Identity e) =>
  Get e (Attributes a1 s)
attributes =
  do c <- attributesInfoUnexpectedEof !- word16be
     i <- (_AttributesAttributeError #) !!- replicateO (\n -> ((,) n) !!- attribute) c
     return (Attributes c i)
-}
