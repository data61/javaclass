{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Java.Class.Attribute(
  Attribute(..)
, HasAttribute(..)
, AttributeError(..)
, HasAttributeError(..)
, AsAttributeError(..)
, attributeUnexpectedEof
, attributeNameIndexUnexpectedEof
, attributeLengthUnexpectedEof
, getAttribute
) where

import Control.Replicate(replicateO)
import Data.Tickle(Get, (!-), word32be, word16be, word8)
import Data.Word(Word8, Word16, Word32)
import Papa

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7 4.7. Attributes>
data Attribute a =
  Attribute {
    _attribute_name_index ::
      Word16
  , _attribute_length ::
      Word32
  , _attribute_info ::
      a Word8
  }

deriving instance Eq (a Word8) => Eq (Attribute a)
deriving instance Ord (a Word8) => Ord (Attribute a)
deriving instance Show (a Word8) => Show (Attribute a)

makeClassy ''Attribute

data AttributeError =
  AttributeNameIndexUnexpectedEof
  | AttributeLengthUnexpectedEof
  | AttributeUnexpectedEof Word32
  deriving (Eq, Ord, Show)

makeClassy ''AttributeError
makeClassyPrisms ''AttributeError

attributeUnexpectedEof ::
  AsAttributeError t =>
  Word32
  -> t  
attributeUnexpectedEof =
  (_AttributeUnexpectedEof #)

attributeNameIndexUnexpectedEof ::
  AsAttributeError t =>
  t
attributeNameIndexUnexpectedEof =
  _AttributeNameIndexUnexpectedEof # ()

attributeLengthUnexpectedEof ::
  AsAttributeError t =>
  t
attributeLengthUnexpectedEof =
  _AttributeLengthUnexpectedEof # ()
     
getAttribute ::
  (AsAttributeError e, Cons (a Word8) (a Word8) Word8 Word8, AsEmpty (a Word8)) =>
  Get e (Attribute a)
getAttribute =
  do n <- attributeNameIndexUnexpectedEof !- word16be
     l <- attributeLengthUnexpectedEof !- word32be
     a <- replicateO (\x -> _AttributeUnexpectedEof # x !- word8) l
     return (Attribute n l a)
