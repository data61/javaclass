{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Java.Class.Attribute {-(
  Attribute(..)
, AttributeError(..)
, AsAttributeNameIndexUnexpectedEof(..)
, attributeNameIndexUnexpectedEof
, AsAttributeLengthUnexpectedEof(..)
, attributeLengthUnexpectedEof
, AsAttributeUnexpectedEof(..)
, attribute
) -} where

import Control.Applicative(Applicative)
import Control.Lens(AsEmpty, Cons, Optic', Choice, prism', ( # ))
import Control.Monad(Monad(return))
import Control.Replicate(replicateO)
import Data.Eq(Eq)
import Data.Functor.Identity(Identity)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Data.Tagged(Tagged)
import Data.Tickle(Get, (!-), word32be, word16be, word8)
import Data.Word(Word8, Word16, Word32)
import Prelude(Show)

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7 4.7. Attributes>
data Attribute a =
  Attribute
    Word16 -- attribute_name_index
    Word32 -- attribute_length
    (a Word8)

deriving instance Eq (a Word8) => Eq (Attribute a)
deriving instance Ord (a Word8) => Ord (Attribute a)
deriving instance Show (a Word8) => Show (Attribute a)

data AttributeError =
  AttributeNameIndexUnexpectedEof
  | AttributeLengthUnexpectedEof
  | AttributeUnexpectedEof Word32
  deriving (Eq, Ord, Show)

{-
class AsAttributeNameIndexUnexpectedEof p f s where
  _AttributeNameIndexUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsAttributeNameIndexUnexpectedEof p f AttributeError where
  _AttributeNameIndexUnexpectedEof =
    prism'
      (\() -> AttributeNameIndexUnexpectedEof)
      (\e -> case e of
               AttributeNameIndexUnexpectedEof -> Just ()
               _ -> Nothing)
       
attributeNameIndexUnexpectedEof ::
  AsAttributeNameIndexUnexpectedEof Tagged Identity t =>
  t
attributeNameIndexUnexpectedEof =
  _AttributeNameIndexUnexpectedEof # ()

class AsAttributeLengthUnexpectedEof p f s where
  _AttributeLengthUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsAttributeLengthUnexpectedEof p f AttributeError where
  _AttributeLengthUnexpectedEof =
    prism'
      (\() -> AttributeLengthUnexpectedEof)
      (\e -> case e of
               AttributeLengthUnexpectedEof -> Just ()
               _ -> Nothing)
       
attributeLengthUnexpectedEof ::
  AsAttributeLengthUnexpectedEof Tagged Identity t =>
  t
attributeLengthUnexpectedEof =
  _AttributeLengthUnexpectedEof # ()

class AsAttributeUnexpectedEof p f s where
  _AttributeUnexpectedEof :: 
    Optic' p f s Word32

instance (Choice p, Applicative f) => AsAttributeUnexpectedEof p f AttributeError where
  _AttributeUnexpectedEof =
    prism'
      AttributeUnexpectedEof
      (\e -> case e of
               AttributeUnexpectedEof w -> Just w
               _ -> Nothing)
       
attribute ::
  (AsEmpty (a Word8), Cons (a Word8) (a Word8) Word8 Word8,
  AsAttributeNameIndexUnexpectedEof Tagged Identity e,
  AsAttributeLengthUnexpectedEof Tagged Identity e,
  AsAttributeUnexpectedEof Tagged Identity e) =>
  Get e (Attribute a)
attribute =
  do n <- attributeNameIndexUnexpectedEof !- word16be
     l <- attributeLengthUnexpectedEof !- word32be
     a <- replicateO (\x -> _AttributeUnexpectedEof # x !- word8) l
     return (Attribute n l a)
-}
