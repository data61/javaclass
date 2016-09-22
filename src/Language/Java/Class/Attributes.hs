{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Java.Class.Attributes(
  Attributes(..)
, HasAttributes(..)
, AttributesError(..)
, HasAttributesError(..)
, AsAttributesError(..)
, attributesInfoUnexpectedEof
, attributesAttributeError
, getAttributes
) where

import Control.Replicate(replicateO)
import Data.Tickle(Get, word16be, (!!-), (!-))
import Data.Word(Word8, Word16)
import Language.Java.Class.Attribute(Attribute, AttributeError, getAttribute)
import Prelude(Show)
import Papa

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1 4.1. The ClassFile Structure>
data Attributes a s =
  Attributes {
    _attribute_count :: 
      Word16
  , _attrs ::
      s (Attribute a)
  }

deriving instance Eq (s (Attribute a)) => Eq (Attributes a s)
deriving instance Ord (s (Attribute a)) => Ord (Attributes a s)
deriving instance Show (s (Attribute a)) => Show (Attributes a s)

makeClassy ''Attributes

data AttributesError =
  AttributesInfoUnexpectedEof
  | AttributesAttributeError Word16 AttributeError
  deriving (Eq, Ord, Show)

makeClassy ''AttributesError
makeClassyPrisms ''AttributesError

attributesInfoUnexpectedEof ::
  AsAttributesError t =>
  t
attributesInfoUnexpectedEof =
  _AttributesInfoUnexpectedEof # ()

attributesAttributeError ::
  AsAttributesError t =>
  Word16
  -> AttributeError
  -> t
attributesAttributeError c a =
  _AttributesAttributeError # (c, a)

getAttributes ::
  (AsAttributesError e, Cons (s (Attribute a1)) (s (Attribute a1)) (Attribute a) (Attribute a), Cons (a Word8) (a Word8) Word8 Word8, AsEmpty (s (Attribute a1)), AsEmpty (a Word8)) =>
  Get e (Attributes a1 s)

getAttributes =
  do c <- attributesInfoUnexpectedEof !- word16be
     i <- (_AttributesAttributeError #) !!- replicateO (\n -> ((,) n) !!- getAttribute) c
     return (Attributes c i)
