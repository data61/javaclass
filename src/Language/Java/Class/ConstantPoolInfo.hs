{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Java.Class.ConstantPoolInfo {- (
  ConstantPoolInfo(..)
, AsConstantClass(..)
, AsFieldRef(..)
, AsMethodRef(..)
, AsInterfaceMethodRef(..)
, AsConstantString(..)
, AsConstantInteger(..)
, AsConstantFloat(..)
, AsConstantLong(..)
, AsConstantDouble(..)
, AsNameAndType(..)
, AsUtf8(..)
, ConstantPoolInfoError(..)
, AsConstantPoolInfoTagUnexpectedEof(..)
, constantPoolInfoTagUnexpectedEof
, AsConstantPoolInfoUtf8LengthUnexpectedEof(..)
, constantPoolInfoUtf8LengthUnexpectedEof
, AsConstantPoolInfoUtf8UnexpectedEof(..)
, AsConstantPoolInvalidJavaString(..)
, AsConstantPoolInfoConstantIntegerUnexpectedEof(..)
, constantPoolInfoConstantIntegerUnexpectedEof
, AsConstantPoolInfoConstantFloatUnexpectedEof(..)
, constantPoolInfoConstantFloatUnexpectedEof
, AsConstantPoolInfoConstantLongUnexpectedEof(..)
, constantPoolInfoConstantLongUnexpectedEof
, AsConstantPoolInfoConstantDoubleUnexpectedEof(..)
, constantPoolInfoConstantDoubleUnexpectedEof
, AsConstantPoolInfoConstantClassUnexpectedEof(..)
, constantPoolInfoConstantClassUnexpectedEof
, AsConstantPoolInfoConstantStringUnexpectedEof(..)
, constantPoolInfoConstantStringUnexpectedEof
, AsConstantPoolInfoFieldRef1UnexpectedEof(..)
, constantPoolInfoFieldRef1UnexpectedEof
, AsConstantPoolInfoFieldRef2UnexpectedEof(..)
, constantPoolInfoFieldRef2UnexpectedEof
, AsConstantPoolInfoMethodRef1UnexpectedEof(..)
, constantPoolInfoMethodRef1UnexpectedEof
, AsConstantPoolInfoMethodRef2UnexpectedEof(..)
, constantPoolInfoMethodRef2UnexpectedEof
, AsConstantPoolInfoInterfaceMethodRef1UnexpectedEof(..)
, constantPoolInfoInterfaceMethodRef1UnexpectedEof
, AsConstantPoolInfoInterfaceMethodRef2UnexpectedEof(..)
, constantPoolInfoInterfaceMethodRef2UnexpectedEof
, AsConstantPoolInfoNameAndType1UnexpectedEof(..)
, constantPoolInfoNameAndType1UnexpectedEof
, AsConstantPoolInfoNameAndType2UnexpectedEof(..)
, constantPoolInfoNameAndType2UnexpectedEof
, AsConstantPoolInfoInvalidConstantPoolTag(..)
, constantPoolInfo
, constantPoolInfo'
) -} where

import Control.Applicative(Applicative)
import Control.Category((.))
import Control.Lens(Optic', Choice, Cons, AsEmpty(_Empty), uncons, prism', (<|), ( # ))
import Control.Monad(Monad(return, (>>=)))
import Control.Replicate(replicateO)
import Data.Bifunctor(bimap)
import Data.Bits(Bits, shift, (.&.))
import Data.Bool(bool, (&&))
import Data.Char(Char, chr)
import Data.Eq(Eq((==)))
import Data.Functor.Identity(Identity)
import Data.Int(Int32, Int64)
import Data.Maybe(Maybe(Just, Nothing), fromMaybe)
import Data.Ord(Ord)
import Data.Tagged(Tagged)
import Data.Tickle((!-), Get, failGet, word8, word16be, int32be, float32be, int64be, float64be)
import Data.Tuple(curry, uncurry)
import Data.Word(Word8, Word16)
import Prelude(Show, Num((+)), Integral, fromIntegral, Float, Double)

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4-140 Table 4.3. Constant pool tags>
data ConstantPoolInfo p =
  ConstantClass Word16
  | FieldRef Word16 Word16
  | MethodRef Word16 Word16
  | InterfaceMethodRef Word16 Word16
  | ConstantString Word16
  | ConstantInteger Int32
  | ConstantFloat Float
  | ConstantLong Int64
  | ConstantDouble Double
  | NameAndType Word16 Word16
  | Utf8 Word16 (p Char)

deriving instance Eq (p Char) => Eq (ConstantPoolInfo p)
deriving instance Ord (p Char) => Ord (ConstantPoolInfo p)
deriving instance Show (p Char) => Show (ConstantPoolInfo p)

data ConstantPoolInfoError c =
  ConstantPoolInfoTagUnexpectedEof
  | ConstantPoolInfoUtf8LengthUnexpectedEof
  | ConstantPoolInfoUtf8UnexpectedEof Word16
  | ConstantPoolInvalidJavaString (c Word8)
  | ConstantPoolInfoConstantIntegerUnexpectedEof
  | ConstantPoolInfoConstantFloatUnexpectedEof
  | ConstantPoolInfoConstantLongUnexpectedEof
  | ConstantPoolInfoConstantDoubleUnexpectedEof
  | ConstantPoolInfoConstantClassUnexpectedEof
  | ConstantPoolInfoConstantStringUnexpectedEof
  | ConstantPoolInfoFieldRef1UnexpectedEof
  | ConstantPoolInfoFieldRef2UnexpectedEof
  | ConstantPoolInfoMethodRef1UnexpectedEof
  | ConstantPoolInfoMethodRef2UnexpectedEof
  | ConstantPoolInfoInterfaceMethodRef1UnexpectedEof
  | ConstantPoolInfoInterfaceMethodRef2UnexpectedEof
  | ConstantPoolInfoNameAndType1UnexpectedEof
  | ConstantPoolInfoNameAndType2UnexpectedEof
  | ConstantPoolInfoInvalidConstantPoolTag Word8

deriving instance Eq (c Word8) => Eq (ConstantPoolInfoError c)
deriving instance Ord (c Word8) => Ord (ConstantPoolInfoError c)
deriving instance Show (c Word8) => Show (ConstantPoolInfoError c)

{-
class AsConstantClass p f s where
  _ConstantClass ::
    Optic' p f s Word16

instance (Choice p', Applicative f) => AsConstantClass p' f (ConstantPoolInfo p) where
  _ConstantClass =
    prism'
      ConstantClass
      (\p -> case p of
               ConstantClass w -> Just w
               _ -> Nothing)

class AsFieldRef p f s where
  _FieldRef ::
    Optic' p f s (Word16, Word16)

instance (Choice p', Applicative f) => AsFieldRef p' f (ConstantPoolInfo p) where
  _FieldRef =
    prism'
      (uncurry FieldRef)
      (\p -> case p of
               FieldRef w1 w2 -> Just (w1, w2)
               _ -> Nothing)

class AsMethodRef p f s where
  _MethodRef ::
    Optic' p f s (Word16, Word16)

instance (Choice p', Applicative f) => AsMethodRef p' f (ConstantPoolInfo p) where
  _MethodRef =
    prism'
      (uncurry MethodRef)
      (\p -> case p of
               MethodRef w1 w2 -> Just (w1, w2)
               _ -> Nothing)

class AsInterfaceMethodRef p f s where
  _InterfaceMethodRef ::
    Optic' p f s (Word16, Word16)

instance (Choice p', Applicative f) => AsInterfaceMethodRef p' f (ConstantPoolInfo p) where
  _InterfaceMethodRef =
    prism'
      (uncurry InterfaceMethodRef)
      (\p -> case p of
               InterfaceMethodRef w1 w2 -> Just (w1, w2)
               _ -> Nothing)

class AsConstantString p f s where
  _ConstantString ::
    Optic' p f s Word16

instance (Choice p', Applicative f) => AsConstantString p' f (ConstantPoolInfo p) where
  _ConstantString =
    prism'
      ConstantString
      (\p -> case p of
               ConstantString w -> Just w
               _ -> Nothing)

class AsConstantInteger p f s where
  _ConstantInteger ::
    Optic' p f s Int32

instance (Choice p', Applicative f) => AsConstantInteger p' f (ConstantPoolInfo p) where
  _ConstantInteger =
    prism'
      ConstantInteger
      (\p -> case p of
               ConstantInteger w -> Just w
               _ -> Nothing)

class AsConstantFloat p f s where
  _ConstantFloat ::
    Optic' p f s Float

instance (Choice p', Applicative f) => AsConstantFloat p' f (ConstantPoolInfo p) where
  _ConstantFloat =
    prism'
      ConstantFloat
      (\p -> case p of
               ConstantFloat w -> Just w
               _ -> Nothing)

class AsConstantLong p f s where
  _ConstantLong ::
    Optic' p f s Int64

instance (Choice p', Applicative f) => AsConstantLong p' f (ConstantPoolInfo p) where
  _ConstantLong =
    prism'
      ConstantLong
      (\p -> case p of
               ConstantLong w -> Just w
               _ -> Nothing)

class AsConstantDouble p f s where
  _ConstantDouble ::
    Optic' p f s Double

instance (Choice p', Applicative f) => AsConstantDouble p' f (ConstantPoolInfo p) where
  _ConstantDouble =
    prism'
      ConstantDouble
      (\p -> case p of
               ConstantDouble w -> Just w
               _ -> Nothing)

class AsNameAndType p f s where
  _NameAndType ::
    Optic' p f s (Word16, Word16)

instance (Choice p', Applicative f) => AsNameAndType p' f (ConstantPoolInfo p) where
  _NameAndType =
    prism'
      (uncurry NameAndType)
      (\p -> case p of
               NameAndType w1 w2 -> Just (w1, w2)
               _ -> Nothing)

class AsUtf8 p f s where
  _Utf8 ::
    Optic' p f (s q) (Word16, q Char)

instance (Choice p', Applicative f) => AsUtf8 p' f ConstantPoolInfo where
  _Utf8 =
    prism'
      (uncurry Utf8)
      (\p -> case p of
               Utf8 w1 w2 -> Just (w1, w2)
               _ -> Nothing)

class AsConstantPoolInfoTagUnexpectedEof p f s where
  _ConstantPoolInfoTagUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsConstantPoolInfoTagUnexpectedEof p f (ConstantPoolInfoError c) where
  _ConstantPoolInfoTagUnexpectedEof =
    prism'
      (\() -> ConstantPoolInfoTagUnexpectedEof)
      (\e -> case e of
               ConstantPoolInfoTagUnexpectedEof -> Just ()
               _ -> Nothing)

constantPoolInfoTagUnexpectedEof ::
  AsConstantPoolInfoTagUnexpectedEof Tagged Identity t =>
  t
constantPoolInfoTagUnexpectedEof =
  _ConstantPoolInfoTagUnexpectedEof # ()

class AsConstantPoolInfoUtf8LengthUnexpectedEof p f s where
  _ConstantPoolInfoUtf8LengthUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsConstantPoolInfoUtf8LengthUnexpectedEof p f (ConstantPoolInfoError c) where
  _ConstantPoolInfoUtf8LengthUnexpectedEof =
    prism'
      (\() -> ConstantPoolInfoUtf8LengthUnexpectedEof)
      (\e -> case e of
               ConstantPoolInfoUtf8LengthUnexpectedEof -> Just ()
               _ -> Nothing)

constantPoolInfoUtf8LengthUnexpectedEof ::
  AsConstantPoolInfoUtf8LengthUnexpectedEof Tagged Identity t =>
  t
constantPoolInfoUtf8LengthUnexpectedEof =
  _ConstantPoolInfoUtf8LengthUnexpectedEof # ()

class AsConstantPoolInfoUtf8UnexpectedEof p f s where
  _ConstantPoolInfoUtf8UnexpectedEof :: 
    Optic' p f s Word16

instance (Choice p, Applicative f) => AsConstantPoolInfoUtf8UnexpectedEof p f (ConstantPoolInfoError c) where
  _ConstantPoolInfoUtf8UnexpectedEof =
    prism'
      ConstantPoolInfoUtf8UnexpectedEof
      (\e -> case e of
               ConstantPoolInfoUtf8UnexpectedEof w -> Just w
               _ -> Nothing)

class AsConstantPoolInvalidJavaString p f s where
  _ConstantPoolInvalidJavaString :: 
    Optic' p f (s c) (c Word8)

instance (Choice p, Applicative f) => AsConstantPoolInvalidJavaString p f ConstantPoolInfoError where
  _ConstantPoolInvalidJavaString =
    prism'
      ConstantPoolInvalidJavaString
      (\e -> case e of
               ConstantPoolInvalidJavaString w -> Just w
               _ -> Nothing)

class AsConstantPoolInfoConstantIntegerUnexpectedEof p f s where
  _ConstantPoolInfoConstantIntegerUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsConstantPoolInfoConstantIntegerUnexpectedEof p f (ConstantPoolInfoError c) where
  _ConstantPoolInfoConstantIntegerUnexpectedEof =
    prism'
      (\() -> ConstantPoolInfoConstantIntegerUnexpectedEof)
      (\e -> case e of
               ConstantPoolInfoConstantIntegerUnexpectedEof -> Just ()
               _ -> Nothing)

constantPoolInfoConstantIntegerUnexpectedEof ::
  AsConstantPoolInfoConstantIntegerUnexpectedEof Tagged Identity t =>
  t
constantPoolInfoConstantIntegerUnexpectedEof =
  _ConstantPoolInfoConstantIntegerUnexpectedEof # ()

class AsConstantPoolInfoConstantFloatUnexpectedEof p f s where
  _ConstantPoolInfoConstantFloatUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsConstantPoolInfoConstantFloatUnexpectedEof p f (ConstantPoolInfoError c) where
  _ConstantPoolInfoConstantFloatUnexpectedEof =
    prism'
      (\() -> ConstantPoolInfoConstantFloatUnexpectedEof)
      (\e -> case e of
               ConstantPoolInfoConstantFloatUnexpectedEof -> Just ()
               _ -> Nothing)

constantPoolInfoConstantFloatUnexpectedEof ::
  AsConstantPoolInfoConstantFloatUnexpectedEof Tagged Identity t =>
  t
constantPoolInfoConstantFloatUnexpectedEof =
  _ConstantPoolInfoConstantFloatUnexpectedEof # ()

class AsConstantPoolInfoConstantLongUnexpectedEof p f s where
  _ConstantPoolInfoConstantLongUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsConstantPoolInfoConstantLongUnexpectedEof p f (ConstantPoolInfoError c) where
  _ConstantPoolInfoConstantLongUnexpectedEof =
    prism'
      (\() -> ConstantPoolInfoConstantLongUnexpectedEof)
      (\e -> case e of
               ConstantPoolInfoConstantLongUnexpectedEof -> Just ()
               _ -> Nothing)

constantPoolInfoConstantLongUnexpectedEof ::
  AsConstantPoolInfoConstantLongUnexpectedEof Tagged Identity t =>
  t
constantPoolInfoConstantLongUnexpectedEof =
  _ConstantPoolInfoConstantLongUnexpectedEof # ()

class AsConstantPoolInfoConstantDoubleUnexpectedEof p f s where
  _ConstantPoolInfoConstantDoubleUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsConstantPoolInfoConstantDoubleUnexpectedEof p f (ConstantPoolInfoError c) where
  _ConstantPoolInfoConstantDoubleUnexpectedEof =
    prism'
      (\() -> ConstantPoolInfoConstantDoubleUnexpectedEof)
      (\e -> case e of
               ConstantPoolInfoConstantDoubleUnexpectedEof -> Just ()
               _ -> Nothing)

constantPoolInfoConstantDoubleUnexpectedEof ::
  AsConstantPoolInfoConstantDoubleUnexpectedEof Tagged Identity t =>
  t
constantPoolInfoConstantDoubleUnexpectedEof =
  _ConstantPoolInfoConstantDoubleUnexpectedEof # ()

class AsConstantPoolInfoConstantClassUnexpectedEof p f s where
  _ConstantPoolInfoConstantClassUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsConstantPoolInfoConstantClassUnexpectedEof p f (ConstantPoolInfoError c) where
  _ConstantPoolInfoConstantClassUnexpectedEof =
    prism'
      (\() -> ConstantPoolInfoConstantClassUnexpectedEof)
      (\e -> case e of
               ConstantPoolInfoConstantClassUnexpectedEof -> Just ()
               _ -> Nothing)

constantPoolInfoConstantClassUnexpectedEof ::
  AsConstantPoolInfoConstantClassUnexpectedEof Tagged Identity t =>
  t
constantPoolInfoConstantClassUnexpectedEof =
  _ConstantPoolInfoConstantClassUnexpectedEof # ()

class AsConstantPoolInfoConstantStringUnexpectedEof p f s where
  _ConstantPoolInfoConstantStringUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsConstantPoolInfoConstantStringUnexpectedEof p f (ConstantPoolInfoError c) where
  _ConstantPoolInfoConstantStringUnexpectedEof =
    prism'
      (\() -> ConstantPoolInfoConstantStringUnexpectedEof)
      (\e -> case e of
               ConstantPoolInfoConstantStringUnexpectedEof -> Just ()
               _ -> Nothing)

constantPoolInfoConstantStringUnexpectedEof ::
  AsConstantPoolInfoConstantStringUnexpectedEof Tagged Identity t =>
  t
constantPoolInfoConstantStringUnexpectedEof =
  _ConstantPoolInfoConstantStringUnexpectedEof # ()

class AsConstantPoolInfoFieldRef1UnexpectedEof p f s where
  _ConstantPoolInfoFieldRef1UnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsConstantPoolInfoFieldRef1UnexpectedEof p f (ConstantPoolInfoError c) where
  _ConstantPoolInfoFieldRef1UnexpectedEof =
    prism'
      (\() -> ConstantPoolInfoFieldRef1UnexpectedEof)
      (\e -> case e of
               ConstantPoolInfoFieldRef1UnexpectedEof -> Just ()
               _ -> Nothing)

constantPoolInfoFieldRef1UnexpectedEof ::
  AsConstantPoolInfoFieldRef1UnexpectedEof Tagged Identity t =>
  t
constantPoolInfoFieldRef1UnexpectedEof =
  _ConstantPoolInfoFieldRef1UnexpectedEof # ()

class AsConstantPoolInfoFieldRef2UnexpectedEof p f s where
  _ConstantPoolInfoFieldRef2UnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsConstantPoolInfoFieldRef2UnexpectedEof p f (ConstantPoolInfoError c) where
  _ConstantPoolInfoFieldRef2UnexpectedEof =
    prism'
      (\() -> ConstantPoolInfoFieldRef2UnexpectedEof)
      (\e -> case e of
               ConstantPoolInfoFieldRef2UnexpectedEof -> Just ()
               _ -> Nothing)

constantPoolInfoFieldRef2UnexpectedEof ::
  AsConstantPoolInfoFieldRef2UnexpectedEof Tagged Identity t =>
  t
constantPoolInfoFieldRef2UnexpectedEof =
  _ConstantPoolInfoFieldRef2UnexpectedEof # ()

class AsConstantPoolInfoMethodRef1UnexpectedEof p f s where
  _ConstantPoolInfoMethodRef1UnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsConstantPoolInfoMethodRef1UnexpectedEof p f (ConstantPoolInfoError c) where
  _ConstantPoolInfoMethodRef1UnexpectedEof =
    prism'
      (\() -> ConstantPoolInfoMethodRef1UnexpectedEof)
      (\e -> case e of
               ConstantPoolInfoMethodRef1UnexpectedEof -> Just ()
               _ -> Nothing)

constantPoolInfoMethodRef1UnexpectedEof ::
  AsConstantPoolInfoMethodRef1UnexpectedEof Tagged Identity t =>
  t
constantPoolInfoMethodRef1UnexpectedEof =
  _ConstantPoolInfoMethodRef1UnexpectedEof # ()

class AsConstantPoolInfoMethodRef2UnexpectedEof p f s where
  _ConstantPoolInfoMethodRef2UnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsConstantPoolInfoMethodRef2UnexpectedEof p f (ConstantPoolInfoError c) where
  _ConstantPoolInfoMethodRef2UnexpectedEof =
    prism'
      (\() -> ConstantPoolInfoMethodRef2UnexpectedEof)
      (\e -> case e of
               ConstantPoolInfoMethodRef2UnexpectedEof -> Just ()
               _ -> Nothing)

constantPoolInfoMethodRef2UnexpectedEof ::
  AsConstantPoolInfoMethodRef2UnexpectedEof Tagged Identity t =>
  t
constantPoolInfoMethodRef2UnexpectedEof =
  _ConstantPoolInfoMethodRef2UnexpectedEof # ()

class AsConstantPoolInfoInterfaceMethodRef1UnexpectedEof p f s where
  _ConstantPoolInfoInterfaceMethodRef1UnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsConstantPoolInfoInterfaceMethodRef1UnexpectedEof p f (ConstantPoolInfoError c) where
  _ConstantPoolInfoInterfaceMethodRef1UnexpectedEof =
    prism'
      (\() -> ConstantPoolInfoInterfaceMethodRef1UnexpectedEof)
      (\e -> case e of
               ConstantPoolInfoInterfaceMethodRef1UnexpectedEof -> Just ()
               _ -> Nothing)

constantPoolInfoInterfaceMethodRef1UnexpectedEof ::
  AsConstantPoolInfoInterfaceMethodRef1UnexpectedEof Tagged Identity t =>
  t
constantPoolInfoInterfaceMethodRef1UnexpectedEof =
  _ConstantPoolInfoInterfaceMethodRef1UnexpectedEof # ()

class AsConstantPoolInfoInterfaceMethodRef2UnexpectedEof p f s where
  _ConstantPoolInfoInterfaceMethodRef2UnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsConstantPoolInfoInterfaceMethodRef2UnexpectedEof p f (ConstantPoolInfoError c) where
  _ConstantPoolInfoInterfaceMethodRef2UnexpectedEof =
    prism'
      (\() -> ConstantPoolInfoInterfaceMethodRef2UnexpectedEof)
      (\e -> case e of
               ConstantPoolInfoInterfaceMethodRef2UnexpectedEof -> Just ()
               _ -> Nothing)

constantPoolInfoInterfaceMethodRef2UnexpectedEof ::
  AsConstantPoolInfoInterfaceMethodRef2UnexpectedEof Tagged Identity t =>
  t
constantPoolInfoInterfaceMethodRef2UnexpectedEof =
  _ConstantPoolInfoInterfaceMethodRef2UnexpectedEof # ()

class AsConstantPoolInfoNameAndType1UnexpectedEof p f s where
  _ConstantPoolInfoNameAndType1UnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsConstantPoolInfoNameAndType1UnexpectedEof p f (ConstantPoolInfoError c) where
  _ConstantPoolInfoNameAndType1UnexpectedEof =
    prism'
      (\() -> ConstantPoolInfoNameAndType1UnexpectedEof)
      (\e -> case e of
               ConstantPoolInfoNameAndType1UnexpectedEof -> Just ()
               _ -> Nothing)

constantPoolInfoNameAndType1UnexpectedEof ::
  AsConstantPoolInfoNameAndType1UnexpectedEof Tagged Identity t =>
  t
constantPoolInfoNameAndType1UnexpectedEof =
  _ConstantPoolInfoNameAndType1UnexpectedEof # ()

class AsConstantPoolInfoNameAndType2UnexpectedEof p f s where
  _ConstantPoolInfoNameAndType2UnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsConstantPoolInfoNameAndType2UnexpectedEof p f (ConstantPoolInfoError c) where
  _ConstantPoolInfoNameAndType2UnexpectedEof =
    prism'
      (\() -> ConstantPoolInfoNameAndType2UnexpectedEof)
      (\e -> case e of
               ConstantPoolInfoNameAndType2UnexpectedEof -> Just ()
               _ -> Nothing)

constantPoolInfoNameAndType2UnexpectedEof ::
  AsConstantPoolInfoNameAndType2UnexpectedEof Tagged Identity t =>
  t
constantPoolInfoNameAndType2UnexpectedEof =
  _ConstantPoolInfoNameAndType2UnexpectedEof # ()

class AsConstantPoolInfoInvalidConstantPoolTag p f s where
  _ConstantPoolInfoInvalidConstantPoolTag :: 
    Optic' p f s Word8

instance (Choice p, Applicative f) => AsConstantPoolInfoInvalidConstantPoolTag p f (ConstantPoolInfoError c) where
  _ConstantPoolInfoInvalidConstantPoolTag =
    prism'
      ConstantPoolInfoInvalidConstantPoolTag
      (\e -> case e of
               ConstantPoolInfoInvalidConstantPoolTag w -> Just w
               _ -> Nothing)

getJavaString ::
  (Integral a, Monad m, AsEmpty b, AsEmpty (m b), Cons s s a a,
  Cons b b Char Char, Bits a) =>
  s
  -> m b
getJavaString q =
  let empty :: AsEmpty t => t
      empty = _Empty # ()
  in case uncons q of
       Nothing ->
         return empty
       Just (x, rest) ->
         bool
           (case uncons rest of
             Nothing ->
               empty
             Just (y, rest2) ->
               bool
                 (case uncons rest2 of
                   Nothing ->
                     empty
                   Just (z, rest3) ->
                     bool
                       empty
                       (let i = ((fromIntegral x .&. 0x0F) `shift` 12 + (fromIntegral y .&. 0x3F) `shift` 6 + (fromIntegral z .&. 0x3F))
                        in getJavaString rest3 >>= return . (chr i <|))
                       ((x .&. 0xF0) == 0xE0 && ((y .&. 0xC0) == 0x80) && ((z .&. 0xC0) == 0x80)))
                 (let i = (fromIntegral x .&. 0x1F) `shift` 6 + (fromIntegral y .&. 0x3F)
                  in getJavaString rest2 >>= return . (chr i <|))
                 ((x .&. 0xE0) == 0xC0 && ((y .&. 0xC0) == 0x80)))
           (getJavaString rest >>= return . (chr (fromIntegral x) <|))
           ((x .&. 0x80) == 0)

constantPoolInfo ::
  (AsEmpty (c Word8), AsEmpty (q Char),
  Cons
    (c Word8)
    (c Word8)
    Word8
    Word8,
  Cons (q Char) (q Char) Char Char,
  AsConstantPoolInfoInvalidConstantPoolTag
    Tagged Identity (s c),
  AsConstantPoolInfoNameAndType2UnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoNameAndType1UnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoInterfaceMethodRef1UnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoMethodRef2UnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoMethodRef1UnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoFieldRef2UnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoFieldRef1UnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoConstantStringUnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoConstantClassUnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoConstantDoubleUnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoConstantLongUnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoConstantFloatUnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoConstantIntegerUnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInvalidJavaString Tagged Identity s,
  AsConstantPoolInfoUtf8UnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoUtf8LengthUnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoTagUnexpectedEof
    Tagged Identity (s c),
  AsUtf8 Tagged Identity s1,
  AsNameAndType Tagged Identity (s1 q),
  AsConstantDouble Tagged Identity (s1 q),
  AsConstantLong Tagged Identity (s1 q),
  AsConstantFloat Tagged Identity (s1 q),
  AsConstantInteger Tagged Identity (s1 q),
  AsConstantString Tagged Identity (s1 q),
  AsInterfaceMethodRef Tagged Identity (s1 q),
  AsMethodRef Tagged Identity (s1 q),
  AsFieldRef Tagged Identity (s1 q),
  AsConstantClass Tagged Identity (s1 q)) =>
  Get (s c) (s1 q)
constantPoolInfo =
  constantPoolInfo' (\_ -> Nothing)

constantPoolInfo' ::
  (AsEmpty (c Word8), AsEmpty (q Char),
  Cons
    (c Word8)
    (c Word8)
    Word8
    Word8,
  Cons (q Char) (q Char) Char Char,
  AsConstantPoolInfoInvalidConstantPoolTag
    Tagged Identity (s c),
  AsConstantPoolInfoNameAndType2UnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoNameAndType1UnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoInterfaceMethodRef1UnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoMethodRef2UnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoMethodRef1UnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoFieldRef2UnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoFieldRef1UnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoConstantStringUnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoConstantClassUnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoConstantDoubleUnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoConstantLongUnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoConstantFloatUnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoConstantIntegerUnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInvalidJavaString Tagged Identity s,
  AsConstantPoolInfoUtf8UnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoUtf8LengthUnexpectedEof
    Tagged Identity (s c),
  AsConstantPoolInfoTagUnexpectedEof
    Tagged Identity (s c),
  AsUtf8 Tagged Identity s1,
  AsNameAndType Tagged Identity (s1 q),
  AsConstantDouble Tagged Identity (s1 q),
  AsConstantLong Tagged Identity (s1 q),
  AsConstantFloat Tagged Identity (s1 q),
  AsConstantInteger Tagged Identity (s1 q),
  AsConstantString Tagged Identity (s1 q),
  AsInterfaceMethodRef Tagged Identity (s1 q),
  AsMethodRef Tagged Identity (s1 q),
  AsFieldRef Tagged Identity (s1 q),
  AsConstantClass Tagged Identity (s1 q)) =>
  (Word8 -> Maybe (Get (s c) (s1 q))) 
  -> Get (s c) (s1 q)
constantPoolInfo' f =
  let eset = bimap . return
      two16 t e1 e2 =
        do c <- e1 !- word16be
           n <- e2 !- word16be
           return (t c n)        
  in do t <- constantPoolInfoTagUnexpectedEof !- word8
        case t of 
          1 ->
            do l <- constantPoolInfoUtf8LengthUnexpectedEof !- word16be
               b <- replicateO (\n -> _ConstantPoolInfoUtf8UnexpectedEof # n !- word8) l
               case getJavaString b of
                 Nothing -> failGet (_ConstantPoolInvalidJavaString # b)
                 Just s -> return (_Utf8 # (l, s))
          3 ->
            eset constantPoolInfoConstantIntegerUnexpectedEof (_ConstantInteger #) int32be
          4 ->
            eset constantPoolInfoConstantFloatUnexpectedEof (_ConstantFloat #) float32be
          5 ->
            eset constantPoolInfoConstantLongUnexpectedEof (_ConstantLong #) int64be
          6 ->
            eset constantPoolInfoConstantDoubleUnexpectedEof (_ConstantDouble #) float64be
          7 ->
            eset constantPoolInfoConstantClassUnexpectedEof (_ConstantClass #) word16be
          8 ->
            eset constantPoolInfoConstantStringUnexpectedEof (_ConstantString #) word16be
          9 ->
            two16 (curry (_FieldRef #)) constantPoolInfoFieldRef1UnexpectedEof constantPoolInfoFieldRef2UnexpectedEof
          10 ->
            two16 (curry (_MethodRef #)) constantPoolInfoMethodRef1UnexpectedEof constantPoolInfoMethodRef2UnexpectedEof
          11 ->
            two16 (curry (_InterfaceMethodRef #)) constantPoolInfoInterfaceMethodRef1UnexpectedEof constantPoolInfoMethodRef2UnexpectedEof
          12 ->
            two16 (curry (_NameAndType #)) constantPoolInfoNameAndType1UnexpectedEof constantPoolInfoNameAndType2UnexpectedEof
          _ ->
            fromMaybe (failGet (_ConstantPoolInfoInvalidConstantPoolTag # t)) (f t) 
-}
