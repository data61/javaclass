{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Java.Class.ConstantPool(
  ConstantPool(..)
, AsConstantPool(..)
, ConstantPoolError(..)
, AsConstantPoolCountUnexpectedEof(..)
, constantPoolCountUnexpectedEof
, AsConstantPoolConstantPoolInfoError(..)
, constantPool
) where

import Control.Applicative(Applicative, liftA2)
import Control.Category((.), id)
import Control.Lens(Choice, Optic', Cons, AsEmpty, Profunctor, prism', iso, isn't, ( # ))
import Control.Lens.Internal.Prism
import Control.Monad(Monad(return))
import Control.Replicate(replicateN)
import Data.Bool(bool, (&&))
import Data.Char(Char)
import Data.Eq(Eq)
import Data.Functor((<$>), Functor)
import Data.Functor.Identity(Identity)
import Data.Int(Int64)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Data.Tagged(Tagged)
import Data.Tickle(Get, word16be, (!-), (!!-))
import Data.Word(Word8, Word16)
import Language.Java.Class.ConstantPoolInfo(AsConstantPoolInfoTagUnexpectedEof(_ConstantPoolInfoTagUnexpectedEof), AsConstantPoolInfoUtf8LengthUnexpectedEof(_ConstantPoolInfoUtf8LengthUnexpectedEof), AsConstantPoolInfoUtf8UnexpectedEof(_ConstantPoolInfoUtf8UnexpectedEof), AsConstantPoolInvalidJavaString(_ConstantPoolInvalidJavaString), AsConstantPoolInfoConstantIntegerUnexpectedEof(_ConstantPoolInfoConstantIntegerUnexpectedEof), AsConstantPoolInfoConstantFloatUnexpectedEof(_ConstantPoolInfoConstantFloatUnexpectedEof), AsConstantPoolInfoConstantLongUnexpectedEof(_ConstantPoolInfoConstantLongUnexpectedEof), AsConstantPoolInfoConstantDoubleUnexpectedEof(_ConstantPoolInfoConstantDoubleUnexpectedEof), AsConstantPoolInfoConstantClassUnexpectedEof(_ConstantPoolInfoConstantClassUnexpectedEof), AsConstantPoolInfoConstantStringUnexpectedEof(_ConstantPoolInfoConstantStringUnexpectedEof), AsConstantPoolInfoFieldRef1UnexpectedEof(_ConstantPoolInfoFieldRef1UnexpectedEof), AsConstantPoolInfoFieldRef2UnexpectedEof(_ConstantPoolInfoFieldRef2UnexpectedEof), AsConstantPoolInfoMethodRef1UnexpectedEof(_ConstantPoolInfoMethodRef1UnexpectedEof), AsConstantPoolInfoMethodRef2UnexpectedEof(_ConstantPoolInfoMethodRef2UnexpectedEof), AsConstantPoolInfoInterfaceMethodRef1UnexpectedEof(_ConstantPoolInfoInterfaceMethodRef1UnexpectedEof), AsConstantPoolInfoInterfaceMethodRef2UnexpectedEof(_ConstantPoolInfoInterfaceMethodRef2UnexpectedEof), AsConstantPoolInfoNameAndType1UnexpectedEof(_ConstantPoolInfoNameAndType1UnexpectedEof), AsConstantPoolInfoNameAndType2UnexpectedEof(_ConstantPoolInfoNameAndType2UnexpectedEof), AsConstantPoolInfoInvalidConstantPoolTag(_ConstantPoolInfoInvalidConstantPoolTag), ConstantPoolInfo(ConstantLong, ConstantDouble), ConstantPoolInfoError, constantPoolInfo, _ConstantDouble, _ConstantLong, AsUtf8, AsConstantLong, AsConstantFloat, AsConstantInteger, AsConstantString, AsInterfaceMethodRef, AsMethodRef, AsFieldRef, AsConstantClass, AsNameAndType, AsConstantDouble)
import Prelude(Show, Num((-)), Double, subtract)

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4 4.4. The Constant Pool>
data ConstantPool p c =
  ConstantPool
    Word16
    (c (ConstantPoolInfo p))

deriving instance Eq (c (ConstantPoolInfo p)) => Eq (ConstantPool p c)
deriving instance Ord (c (ConstantPoolInfo p)) => Ord (ConstantPool p c)
deriving instance Show (c (ConstantPoolInfo p)) => Show (ConstantPool p c)

class AsConstantPool p f s where
  _ConstantPool ::
    Optic' p f (s p' c) (Word16, c (ConstantPoolInfo p'))

instance (Profunctor p, Functor f) => AsConstantPool p f ConstantPool where
  _ConstantPool =
    iso
      (\(ConstantPool w i) -> (w, i))
      (\(w, i) -> ConstantPool w i)

data ConstantPoolError c =
  ConstantPoolCountUnexpectedEof
  | ConstantPoolConstantPoolInfoError (ConstantPoolInfoError c)

deriving instance Eq (ConstantPoolInfoError c) => Eq (ConstantPoolError c)
deriving instance Ord (ConstantPoolInfoError c) => Ord (ConstantPoolError c)
deriving instance Show (ConstantPoolInfoError c) => Show (ConstantPoolError c)

class AsConstantPoolCountUnexpectedEof p f s where
  _ConstantPoolCountUnexpectedEof :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsConstantPoolCountUnexpectedEof p f (ConstantPoolError c) where
  _ConstantPoolCountUnexpectedEof =
    prism'
      (\() -> ConstantPoolCountUnexpectedEof)
      (\e -> case e of
               ConstantPoolCountUnexpectedEof -> Just ()
               _ -> Nothing)

constantPoolCountUnexpectedEof ::
  AsConstantPoolCountUnexpectedEof Tagged Identity t =>
  t
constantPoolCountUnexpectedEof =
  _ConstantPoolCountUnexpectedEof # ()

class AsConstantPoolConstantPoolInfoError p f s where
  _ConstantPoolConstantPoolInfoError :: 
    Optic' p f (s c) (ConstantPoolInfoError c)

instance (Choice p, Applicative f) => AsConstantPoolConstantPoolInfoError p f ConstantPoolError where
  _ConstantPoolConstantPoolInfoError =
    prism'
      ConstantPoolConstantPoolInfoError
      (\e -> case e of
               ConstantPoolConstantPoolInfoError r -> Just r
               _ -> Nothing)


instance (Choice p, Applicative f) => AsConstantPoolInfoTagUnexpectedEof p f (ConstantPoolError c) where
  _ConstantPoolInfoTagUnexpectedEof =
    _ConstantPoolConstantPoolInfoError . _ConstantPoolInfoTagUnexpectedEof


instance (Choice p, Applicative f) => AsConstantPoolInfoUtf8LengthUnexpectedEof p f (ConstantPoolError c) where
  _ConstantPoolInfoUtf8LengthUnexpectedEof =
    _ConstantPoolConstantPoolInfoError . _ConstantPoolInfoUtf8LengthUnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoUtf8UnexpectedEof p f (ConstantPoolError c) where
  _ConstantPoolInfoUtf8UnexpectedEof =
    _ConstantPoolConstantPoolInfoError . _ConstantPoolInfoUtf8UnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInvalidJavaString p f ConstantPoolError where
  _ConstantPoolInvalidJavaString =
    _ConstantPoolConstantPoolInfoError . _ConstantPoolInvalidJavaString

instance (Choice p, Applicative f) => AsConstantPoolInfoConstantIntegerUnexpectedEof p f (ConstantPoolError c) where
  _ConstantPoolInfoConstantIntegerUnexpectedEof =
    _ConstantPoolConstantPoolInfoError . _ConstantPoolInfoConstantIntegerUnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoConstantFloatUnexpectedEof p f (ConstantPoolError c) where
  _ConstantPoolInfoConstantFloatUnexpectedEof =
    _ConstantPoolConstantPoolInfoError . _ConstantPoolInfoConstantFloatUnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoConstantLongUnexpectedEof p f (ConstantPoolError c) where
  _ConstantPoolInfoConstantLongUnexpectedEof =
    _ConstantPoolConstantPoolInfoError . _ConstantPoolInfoConstantLongUnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoConstantDoubleUnexpectedEof p f (ConstantPoolError c) where
  _ConstantPoolInfoConstantDoubleUnexpectedEof =
    _ConstantPoolConstantPoolInfoError . _ConstantPoolInfoConstantDoubleUnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoConstantClassUnexpectedEof p f (ConstantPoolError c) where
  _ConstantPoolInfoConstantClassUnexpectedEof =
    _ConstantPoolConstantPoolInfoError . _ConstantPoolInfoConstantClassUnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoConstantStringUnexpectedEof p f (ConstantPoolError c) where
  _ConstantPoolInfoConstantStringUnexpectedEof =
    _ConstantPoolConstantPoolInfoError . _ConstantPoolInfoConstantStringUnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoFieldRef1UnexpectedEof p f (ConstantPoolError c) where
  _ConstantPoolInfoFieldRef1UnexpectedEof =
    _ConstantPoolConstantPoolInfoError . _ConstantPoolInfoFieldRef1UnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoFieldRef2UnexpectedEof p f (ConstantPoolError c) where
  _ConstantPoolInfoFieldRef2UnexpectedEof =
    _ConstantPoolConstantPoolInfoError . _ConstantPoolInfoFieldRef2UnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoMethodRef1UnexpectedEof p f (ConstantPoolError c) where
  _ConstantPoolInfoMethodRef1UnexpectedEof =
    _ConstantPoolConstantPoolInfoError . _ConstantPoolInfoMethodRef1UnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoMethodRef2UnexpectedEof p f (ConstantPoolError c) where
  _ConstantPoolInfoMethodRef2UnexpectedEof =
    _ConstantPoolConstantPoolInfoError . _ConstantPoolInfoMethodRef2UnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoInterfaceMethodRef1UnexpectedEof p f (ConstantPoolError c) where
  _ConstantPoolInfoInterfaceMethodRef1UnexpectedEof =
    _ConstantPoolConstantPoolInfoError . _ConstantPoolInfoInterfaceMethodRef1UnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoInterfaceMethodRef2UnexpectedEof p f (ConstantPoolError c) where
  _ConstantPoolInfoInterfaceMethodRef2UnexpectedEof =
    _ConstantPoolConstantPoolInfoError . _ConstantPoolInfoInterfaceMethodRef2UnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoNameAndType1UnexpectedEof p f (ConstantPoolError c) where
  _ConstantPoolInfoNameAndType1UnexpectedEof =
    _ConstantPoolConstantPoolInfoError . _ConstantPoolInfoNameAndType1UnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoNameAndType2UnexpectedEof p f (ConstantPoolError c) where
  _ConstantPoolInfoNameAndType2UnexpectedEof =
    _ConstantPoolConstantPoolInfoError . _ConstantPoolInfoNameAndType2UnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoInvalidConstantPoolTag p f (ConstantPoolError c) where
  _ConstantPoolInfoInvalidConstantPoolTag =
    _ConstantPoolConstantPoolInfoError . _ConstantPoolInfoInvalidConstantPoolTag

constantPool ::
 (AsEmpty (c Word8), AsEmpty (t Char),
  AsEmpty (c1 (ConstantPoolInfo p)),
  Cons (c Word8) (c Word8) Word8 Word8,
  Cons (t Char) (t Char) Char Char,
  Cons
    (c1 (ConstantPoolInfo p))
    (c1 (ConstantPoolInfo p))
    (ConstantPoolInfo t)
    (ConstantPoolInfo t),
  AsConstantPoolCountUnexpectedEof Tagged Identity (s c),
  AsConstantPoolConstantPoolInfoError Tagged Identity s) =>
  Get (s c) (ConstantPool p c1)
constantPool =
  let jump (ConstantLong _) = subtract 1
      jump (ConstantDouble _) = subtract 1
      jump _ = id
  in do constant_pool_count <- constantPoolCountUnexpectedEof !- word16be
        pool <- (_ConstantPoolConstantPoolInfoError #) !!- replicateN (\n -> (\i -> (jump i n, i)) <$> constantPoolInfo) (constant_pool_count - 1)
        return (ConstantPool constant_pool_count pool)

constantPool' ::
  (AsEmpty (c Word8), AsEmpty (q Char),
    AsEmpty (c1 (ConstantPoolInfo p')),
    Cons
      (c Word8)
      (c Word8)
      Word8
      Word8,
    Cons (q Char) (q Char) Char Char,
    Cons
      (c1 (ConstantPoolInfo p'))
      (c1 (ConstantPoolInfo p'))
      (s1 q)
      (s1 q),
    AsUtf8 Tagged Identity s1,
    AsNameAndType Tagged Identity (s1 q),
    AsConstantDouble Tagged Identity (s1 q),
    AsConstantDouble
      (Market Double Double) Identity (s1 q),
    AsConstantLong Tagged Identity (s1 q),
    AsConstantLong
      (Market Int64 Int64)
      Identity
      (s1 q),
    AsConstantFloat Tagged Identity (s1 q),
    AsConstantInteger Tagged Identity (s1 q),
    AsConstantString Tagged Identity (s1 q),
    AsInterfaceMethodRef Tagged Identity (s1 q),
    AsMethodRef Tagged Identity (s1 q),
    AsFieldRef Tagged Identity (s1 q),
    AsConstantClass Tagged Identity (s1 q),
    AsConstantPoolConstantPoolInfoError Tagged Identity s,
    AsConstantPoolCountUnexpectedEof Tagged Identity (s c),
    AsConstantPool Tagged Identity s2) =>
  Get (s c) (s2 p' c1)
constantPool' =
  let jump =
        bool (subtract 1) id . liftA2 (&&) (isn't _ConstantLong) (isn't _ConstantDouble)
  in do constant_pool_count <- constantPoolCountUnexpectedEof !- word16be
        pool <- (_ConstantPoolConstantPoolInfoError #) !!- replicateN (\n -> (\i -> (jump i n, i)) <$> constantPoolInfo) (constant_pool_count - 1)
        return (_ConstantPool # (constant_pool_count, pool))
