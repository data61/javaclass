{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}

module Language.Java.Class.ClassFile(
  ClassFile(..)
, ClassFile'
, ClassFileError(..)
, AsClassFileCafebabeError(..)
, AsClassFileVersionError(..)
, AsClassFileConstantPoolError(..)
, AsClassFileThisAccessFlagsError(..)
, AsClassFileThisClassError(..)
, AsClassFileSuperClassError(..)
, AsClassFileInterfacesError(..)
, AsClassFileFieldsError(..)
, AsClassFileMethodsError(..)
, AsClassFileAttributesError(..)
, AsClassFileUnexpectedInputOnStream(..)
, classFileUnexpectedInputOnStream
, classFile
) where

import Control.Applicative
import Control.Category((.))
import Control.Lens(AsEmpty, Cons, Choice, Optic', prism', ( # ))
import Control.Monad(return)
import Data.Char(Char)
import Data.Eq(Eq)
import Data.Functor.Identity(Identity)
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord)
import Data.Tagged(Tagged)
import Data.Tickle(Get, isEmpty, failGet, (!!-))
import Data.Word(Word8, Word16)
import Language.Java.Class.Attribute(AsAttributeNameIndexUnexpectedEof(_AttributeNameIndexUnexpectedEof), AsAttributeLengthUnexpectedEof(_AttributeLengthUnexpectedEof), AsAttributeUnexpectedEof(_AttributeUnexpectedEof), Attribute)
import Language.Java.Class.Attributes(AsAttributesInfoUnexpectedEof(_AttributesInfoUnexpectedEof), AsAttributesAttributeError(_AttributesAttributeError), Attributes, AttributesError, attributes)
import Language.Java.Class.Method(AsMethodNameIndexUnexpectedEof(_MethodNameIndexUnexpectedEof), AsMethodDescriptorIndexUnexpectedEof(_MethodDescriptorIndexUnexpectedEof), AsMethodAttributeCountUnexpectedEof(_MethodAttributeCountUnexpectedEof), AsMethodAttributeUnexpectedEof(_MethodAttributeUnexpectedEof), AsMethodMethodAccessFlagsError(_MethodMethodAccessFlagsError), AsMethodErrorAttributeError(_MethodErrorAttributeError), Method)
import Language.Java.Class.Methods(AsMethodsInfoUnexpectedEof(_MethodsInfoUnexpectedEof), AsMethodsMethodError(_MethodsMethodError), Methods, MethodsError, methods)
import Language.Java.Class.Field(AsFieldFieldAccessFlagsError(_FieldFieldAccessFlagsError), AsFieldNameIndexUnexpectedEof(_FieldNameIndexUnexpectedEof), AsFieldDescriptorIndexUnexpectedEof(_FieldDescriptorIndexUnexpectedEof), AsFieldAttributeCountUnexpectedEof(_FieldAttributeCountUnexpectedEof), AsFieldAttributeError(_FieldAttributeError), AsFieldErrorAttributeError(_FieldErrorAttributeError), Field)
import Language.Java.Class.Fields(AsFieldsFieldInfoUnexpectedEof(_FieldsFieldInfoUnexpectedEof), AsFieldsFieldError(_FieldsFieldError), Fields, FieldsError, fields)
import Language.Java.Class.Interfaces(AsInterfacesCountUnexpectedEof(_InterfacesCountUnexpectedEof), AsInterfacesUnexpectedEof(_InterfacesUnexpectedEof), Interfaces, InterfacesError, interfaces)
import Language.Java.Class.SuperClass(AsSuperClassUnexpectedEof(_SuperClassUnexpectedEof), SuperClass, SuperClassError, superClass)
import Language.Java.Class.ThisClass(AsThisClassUnexpectedEof(_ThisClassUnexpectedEof), ThisClass, ThisClassError, thisClass)
import Language.Java.Class.ThisAccessFlags(AsThisAccessFlagsUnexpectedEof(_ThisAccessFlagsUnexpectedEof), ThisAccessFlags, ThisAccessFlagsError, thisAccessFlags)
import Language.Java.Class.ConstantPoolInfo(AsConstantPoolInfoTagUnexpectedEof(_ConstantPoolInfoTagUnexpectedEof), AsConstantPoolInfoUtf8LengthUnexpectedEof(_ConstantPoolInfoUtf8LengthUnexpectedEof), AsConstantPoolInfoUtf8UnexpectedEof(_ConstantPoolInfoUtf8UnexpectedEof), AsConstantPoolInvalidJavaString(_ConstantPoolInvalidJavaString), AsConstantPoolInfoConstantIntegerUnexpectedEof(_ConstantPoolInfoConstantIntegerUnexpectedEof), AsConstantPoolInfoConstantFloatUnexpectedEof(_ConstantPoolInfoConstantFloatUnexpectedEof), AsConstantPoolInfoConstantLongUnexpectedEof(_ConstantPoolInfoConstantLongUnexpectedEof), AsConstantPoolInfoConstantDoubleUnexpectedEof(_ConstantPoolInfoConstantDoubleUnexpectedEof), AsConstantPoolInfoConstantClassUnexpectedEof(_ConstantPoolInfoConstantClassUnexpectedEof), AsConstantPoolInfoConstantStringUnexpectedEof(_ConstantPoolInfoConstantStringUnexpectedEof), AsConstantPoolInfoFieldRef1UnexpectedEof(_ConstantPoolInfoFieldRef1UnexpectedEof), AsConstantPoolInfoFieldRef2UnexpectedEof(_ConstantPoolInfoFieldRef2UnexpectedEof), AsConstantPoolInfoMethodRef1UnexpectedEof(_ConstantPoolInfoMethodRef1UnexpectedEof), AsConstantPoolInfoMethodRef2UnexpectedEof(_ConstantPoolInfoMethodRef2UnexpectedEof), AsConstantPoolInfoInterfaceMethodRef1UnexpectedEof(_ConstantPoolInfoInterfaceMethodRef1UnexpectedEof), AsConstantPoolInfoInterfaceMethodRef2UnexpectedEof(_ConstantPoolInfoInterfaceMethodRef2UnexpectedEof), AsConstantPoolInfoNameAndType1UnexpectedEof(_ConstantPoolInfoNameAndType1UnexpectedEof), AsConstantPoolInfoNameAndType2UnexpectedEof(_ConstantPoolInfoNameAndType2UnexpectedEof), AsConstantPoolInfoInvalidConstantPoolTag(_ConstantPoolInfoInvalidConstantPoolTag), ConstantPoolInfo)
import Language.Java.Class.MethodAccessFlags(AsMethodAccessFlagsUnexpectedEof(_MethodAccessFlagsUnexpectedEof))
import Language.Java.Class.ConstantPool(AsConstantPoolCountUnexpectedEof(_ConstantPoolCountUnexpectedEof), AsConstantPoolConstantPoolInfoError(_ConstantPoolConstantPoolInfoError), ConstantPool, ConstantPoolError, constantPool)
import Language.Java.Class.Cafebabe(AsCafebabeUnexpectedEof(_CafebabeUnexpectedEof), AsCafebabeInvalidMagicNumber(_CafebabeInvalidMagicNumber), CafebabeError, cafebabe)
import Language.Java.Class.Version(AsVersionMinorUnexpectedEof(_VersionMinorUnexpectedEof), AsVersionMajorUnexpectedEof(_VersionMajorUnexpectedEof), Version, VersionError, version)
import Prelude(Show)

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1 4.1. The ClassFile Structure >
data ClassFile p c i a f s m b t d u =
  ClassFile
    Version
    (ConstantPool p c)
    ThisAccessFlags
    ThisClass
    SuperClass
    (Interfaces i)
    (Fields a f s)
    (Methods m b t)
    (Attributes d u)

deriving instance (Eq (ConstantPool p c), Eq (Interfaces i), Eq (Fields a f s), Eq (Methods m b t), Eq (Attributes d u)) => Eq (ClassFile p c i a f s m b t d u)
deriving instance (Ord (ConstantPool p c), Ord (Interfaces i), Ord (Fields a f s), Ord (Methods m b t), Ord (Attributes d u)) => Ord (ClassFile p c i a f s m b t d u)
deriving instance (Show (ConstantPool p c), Show (Interfaces i), Show (Fields a f s), Show (Methods m b t), Show (Attributes d u)) => Show (ClassFile p c i a f s m b t d u)

type ClassFile' x =
  ClassFile x x x x x x x x x x x
    
data ClassFileError c =
  ClassFileCafebabeError CafebabeError
  | ClassFileVersionError VersionError
  | ClassFileConstantPoolError (ConstantPoolError c)
  | ClassFileThisAccessFlagsError ThisAccessFlagsError
  | ClassFileThisClassError ThisClassError
  | ClassFileSuperClassError SuperClassError
  | ClassFileInterfacesError InterfacesError
  | ClassFileFieldsError FieldsError
  | ClassFileMethodsError MethodsError
  | ClassFileAttributesError AttributesError
  | ClassFileUnexpectedInputOnStream

deriving instance Eq (ConstantPoolError c) => Eq (ClassFileError c)
deriving instance Ord (ConstantPoolError c) => Ord (ClassFileError c)
deriving instance Show (ConstantPoolError c) => Show (ClassFileError c)

class AsClassFileCafebabeError p f s where
  _ClassFileCafebabeError :: 
    Optic' p f s CafebabeError

instance (Choice p, Applicative f) => AsClassFileCafebabeError p f (ClassFileError c) where
  _ClassFileCafebabeError =
    prism'
      ClassFileCafebabeError
      (\e -> case e of
               ClassFileCafebabeError r -> Just r
               _ -> Nothing)

instance (Choice p, Applicative f) => AsCafebabeUnexpectedEof p f (ClassFileError c) where
  _CafebabeUnexpectedEof =
    _ClassFileCafebabeError . _CafebabeUnexpectedEof

instance (Choice p, Applicative f) => AsCafebabeInvalidMagicNumber p f (ClassFileError c) where
  _CafebabeInvalidMagicNumber =
    _ClassFileCafebabeError . _CafebabeInvalidMagicNumber

class AsClassFileVersionError p f s where
  _ClassFileVersionError :: 
    Optic' p f s VersionError

instance (Choice p, Applicative f) => AsClassFileVersionError p f (ClassFileError c) where
  _ClassFileVersionError =
    prism'
      ClassFileVersionError
      (\e -> case e of
               ClassFileVersionError r -> Just r
               _ -> Nothing)

instance (Choice p, Applicative f) => AsVersionMinorUnexpectedEof p f (ClassFileError c) where
  _VersionMinorUnexpectedEof =
    _ClassFileVersionError . _VersionMinorUnexpectedEof

instance (Choice p, Applicative f) => AsVersionMajorUnexpectedEof p f (ClassFileError c) where
  _VersionMajorUnexpectedEof =
    _ClassFileVersionError . _VersionMajorUnexpectedEof

class AsClassFileConstantPoolError p f s where
  _ClassFileConstantPoolError :: 
    Optic' p f (s c) (ConstantPoolError c)

instance (Choice p, Applicative f) => AsClassFileConstantPoolError p f ClassFileError where
  _ClassFileConstantPoolError =
    prism'
      ClassFileConstantPoolError
      (\e -> case e of
               ClassFileConstantPoolError r -> Just r
               _ -> Nothing)

instance (Choice p, Applicative f) => AsConstantPoolCountUnexpectedEof p f (ClassFileError c) where
  _ConstantPoolCountUnexpectedEof =
    _ClassFileConstantPoolError . _ConstantPoolCountUnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolConstantPoolInfoError p f ClassFileError where
  _ConstantPoolConstantPoolInfoError =
    _ClassFileConstantPoolError . _ConstantPoolConstantPoolInfoError

instance (Choice p, Applicative f) => AsConstantPoolInfoTagUnexpectedEof p f (ClassFileError c) where
  _ConstantPoolInfoTagUnexpectedEof =
    _ClassFileConstantPoolError . _ConstantPoolInfoTagUnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoUtf8LengthUnexpectedEof p f (ClassFileError c) where
  _ConstantPoolInfoUtf8LengthUnexpectedEof =
    _ClassFileConstantPoolError . _ConstantPoolInfoUtf8LengthUnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoUtf8UnexpectedEof p f (ClassFileError c) where
  _ConstantPoolInfoUtf8UnexpectedEof =
    _ClassFileConstantPoolError . _ConstantPoolInfoUtf8UnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInvalidJavaString p f ClassFileError where
  _ConstantPoolInvalidJavaString =
    _ClassFileConstantPoolError . _ConstantPoolInvalidJavaString

instance (Choice p, Applicative f) => AsConstantPoolInfoConstantIntegerUnexpectedEof p f (ClassFileError c) where
  _ConstantPoolInfoConstantIntegerUnexpectedEof =
    _ClassFileConstantPoolError . _ConstantPoolInfoConstantIntegerUnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoConstantFloatUnexpectedEof p f (ClassFileError c) where
  _ConstantPoolInfoConstantFloatUnexpectedEof =
    _ClassFileConstantPoolError . _ConstantPoolInfoConstantFloatUnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoConstantLongUnexpectedEof p f (ClassFileError c) where
  _ConstantPoolInfoConstantLongUnexpectedEof =
    _ClassFileConstantPoolError . _ConstantPoolInfoConstantLongUnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoConstantDoubleUnexpectedEof p f (ClassFileError c) where
  _ConstantPoolInfoConstantDoubleUnexpectedEof =
    _ClassFileConstantPoolError . _ConstantPoolInfoConstantDoubleUnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoConstantClassUnexpectedEof p f (ClassFileError c) where
  _ConstantPoolInfoConstantClassUnexpectedEof =
    _ClassFileConstantPoolError . _ConstantPoolInfoConstantClassUnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoConstantStringUnexpectedEof p f (ClassFileError c) where
  _ConstantPoolInfoConstantStringUnexpectedEof =
    _ClassFileConstantPoolError . _ConstantPoolInfoConstantStringUnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoFieldRef1UnexpectedEof p f (ClassFileError c) where
  _ConstantPoolInfoFieldRef1UnexpectedEof =
    _ClassFileConstantPoolError . _ConstantPoolInfoFieldRef1UnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoFieldRef2UnexpectedEof p f (ClassFileError c) where
  _ConstantPoolInfoFieldRef2UnexpectedEof =
    _ClassFileConstantPoolError . _ConstantPoolInfoFieldRef2UnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoMethodRef1UnexpectedEof p f (ClassFileError c) where
  _ConstantPoolInfoMethodRef1UnexpectedEof =
    _ClassFileConstantPoolError . _ConstantPoolInfoMethodRef1UnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoMethodRef2UnexpectedEof p f (ClassFileError c) where
  _ConstantPoolInfoMethodRef2UnexpectedEof =
    _ClassFileConstantPoolError . _ConstantPoolInfoMethodRef2UnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoInterfaceMethodRef1UnexpectedEof p f (ClassFileError c) where
  _ConstantPoolInfoInterfaceMethodRef1UnexpectedEof =
    _ClassFileConstantPoolError . _ConstantPoolInfoInterfaceMethodRef1UnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoInterfaceMethodRef2UnexpectedEof p f (ClassFileError c) where
  _ConstantPoolInfoInterfaceMethodRef2UnexpectedEof =
    _ClassFileConstantPoolError . _ConstantPoolInfoInterfaceMethodRef2UnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoNameAndType1UnexpectedEof p f (ClassFileError c) where
  _ConstantPoolInfoNameAndType1UnexpectedEof =
    _ClassFileConstantPoolError . _ConstantPoolInfoNameAndType1UnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoNameAndType2UnexpectedEof p f (ClassFileError c) where
  _ConstantPoolInfoNameAndType2UnexpectedEof =
    _ClassFileConstantPoolError . _ConstantPoolInfoNameAndType2UnexpectedEof

instance (Choice p, Applicative f) => AsConstantPoolInfoInvalidConstantPoolTag p f (ClassFileError c) where
  _ConstantPoolInfoInvalidConstantPoolTag =
    _ClassFileConstantPoolError . _ConstantPoolInfoInvalidConstantPoolTag

class AsClassFileThisAccessFlagsError p f s where
  _ClassFileThisAccessFlagsError :: 
    Optic' p f s ThisAccessFlagsError

instance (Choice p, Applicative f) => AsClassFileThisAccessFlagsError p f (ClassFileError c) where
  _ClassFileThisAccessFlagsError =
    prism'
      ClassFileThisAccessFlagsError
      (\e -> case e of
               ClassFileThisAccessFlagsError r -> Just r
               _ -> Nothing)

instance (Choice p, Applicative f) => AsThisAccessFlagsUnexpectedEof p f (ClassFileError c) where
  _ThisAccessFlagsUnexpectedEof =
    _ClassFileThisAccessFlagsError . _ThisAccessFlagsUnexpectedEof

class AsClassFileThisClassError p f s where
  _ClassFileThisClassError :: 
    Optic' p f s ThisClassError

instance (Choice p, Applicative f) => AsClassFileThisClassError p f (ClassFileError c) where
  _ClassFileThisClassError =
    prism'
      ClassFileThisClassError
      (\e -> case e of
               ClassFileThisClassError r -> Just r
               _ -> Nothing)

instance (Choice p, Applicative f) => AsThisClassUnexpectedEof p f (ClassFileError c) where
  _ThisClassUnexpectedEof =
    _ClassFileThisClassError . _ThisClassUnexpectedEof

class AsClassFileSuperClassError p f s where
  _ClassFileSuperClassError :: 
    Optic' p f s SuperClassError

instance (Choice p, Applicative f) => AsClassFileSuperClassError p f (ClassFileError c) where
  _ClassFileSuperClassError =
    prism'
      ClassFileSuperClassError
      (\e -> case e of
               ClassFileSuperClassError r -> Just r
               _ -> Nothing)

instance (Choice p, Applicative f) => AsSuperClassUnexpectedEof p f (ClassFileError c) where
  _SuperClassUnexpectedEof =
    _ClassFileSuperClassError . _SuperClassUnexpectedEof

class AsClassFileInterfacesError p f s where
  _ClassFileInterfacesError :: 
    Optic' p f s InterfacesError

instance (Choice p, Applicative f) => AsClassFileInterfacesError p f (ClassFileError c) where
  _ClassFileInterfacesError =
    prism'
      ClassFileInterfacesError
      (\e -> case e of
               ClassFileInterfacesError r -> Just r
               _ -> Nothing)

instance (Choice p, Applicative f) => AsInterfacesCountUnexpectedEof p f (ClassFileError c) where
  _InterfacesCountUnexpectedEof =
    _ClassFileInterfacesError . _InterfacesCountUnexpectedEof

instance (Choice p, Applicative f) => AsInterfacesUnexpectedEof p f (ClassFileError c) where
  _InterfacesUnexpectedEof =
    _ClassFileInterfacesError . _InterfacesUnexpectedEof

class AsClassFileFieldsError p f s where
  _ClassFileFieldsError :: 
    Optic' p f s FieldsError

instance (Choice p, Applicative f) => AsClassFileFieldsError p f (ClassFileError c) where
  _ClassFileFieldsError =
    prism'
      ClassFileFieldsError
      (\e -> case e of
               ClassFileFieldsError r -> Just r
               _ -> Nothing)

instance (Choice p, Applicative f) => AsFieldsFieldInfoUnexpectedEof p f (ClassFileError c) where
  _FieldsFieldInfoUnexpectedEof =
    _ClassFileFieldsError . _FieldsFieldInfoUnexpectedEof

instance (Choice p, Applicative f) => AsFieldsFieldError p f (ClassFileError c) where
  _FieldsFieldError =
    _ClassFileFieldsError . _FieldsFieldError

instance (p ~ (->), Applicative f) => AsFieldFieldAccessFlagsError p f (ClassFileError c) where
  _FieldFieldAccessFlagsError =
    _ClassFileFieldsError . _FieldFieldAccessFlagsError

instance (p ~ (->), Applicative f) => AsFieldNameIndexUnexpectedEof p f (ClassFileError c) where
  _FieldNameIndexUnexpectedEof =
    _ClassFileFieldsError . _FieldNameIndexUnexpectedEof

instance (p ~ (->), Applicative f) => AsFieldDescriptorIndexUnexpectedEof p f (ClassFileError c) where
  _FieldDescriptorIndexUnexpectedEof =
    _ClassFileFieldsError . _FieldDescriptorIndexUnexpectedEof

instance (p ~ (->), Applicative f) => AsFieldAttributeCountUnexpectedEof p f (ClassFileError c) where
  _FieldAttributeCountUnexpectedEof =
    _ClassFileFieldsError . _FieldAttributeCountUnexpectedEof

instance (p ~ (->), Applicative f) => AsFieldErrorAttributeError p f (ClassFileError c) where
  _FieldErrorAttributeError =
    _ClassFileFieldsError . _FieldErrorAttributeError

instance (p ~ (->), Applicative f) => AsFieldAttributeError p f (ClassFileError c) where
  _FieldAttributeError =
    _ClassFileFieldsError . _FieldAttributeError

class AsClassFileMethodsError p f s where
  _ClassFileMethodsError :: 
    Optic' p f s MethodsError

instance (Choice p, Applicative f) => AsClassFileMethodsError p f (ClassFileError c) where
  _ClassFileMethodsError =
    prism'
      ClassFileMethodsError
      (\e -> case e of
               ClassFileMethodsError r -> Just r
               _ -> Nothing)

instance (Choice p, Applicative f) => AsMethodsInfoUnexpectedEof p f (ClassFileError c) where
  _MethodsInfoUnexpectedEof =
    _ClassFileMethodsError . _MethodsInfoUnexpectedEof
  
instance (Choice p, Applicative f) => AsMethodsMethodError p f (ClassFileError c) where
  _MethodsMethodError =
    _ClassFileMethodsError . _MethodsMethodError

instance (p ~ (->), Applicative f) => AsMethodNameIndexUnexpectedEof p f (ClassFileError c) where
  _MethodNameIndexUnexpectedEof =
    _ClassFileMethodsError . _MethodNameIndexUnexpectedEof

instance (p ~ (->), Applicative f) => AsMethodDescriptorIndexUnexpectedEof p f (ClassFileError c) where
  _MethodDescriptorIndexUnexpectedEof =
    _ClassFileMethodsError . _MethodDescriptorIndexUnexpectedEof

instance (p ~ (->), Applicative f) => AsMethodAttributeCountUnexpectedEof p f (ClassFileError c) where
  _MethodAttributeCountUnexpectedEof =
    _ClassFileMethodsError . _MethodAttributeCountUnexpectedEof

instance (p ~ (->), Applicative f) => AsMethodAttributeUnexpectedEof p f (ClassFileError c) where
  _MethodAttributeUnexpectedEof =
    _ClassFileMethodsError . _MethodAttributeUnexpectedEof

instance (p ~ (->), Applicative f) => AsMethodErrorAttributeError p f (ClassFileError c) where
  _MethodErrorAttributeError =
    _ClassFileMethodsError . _MethodErrorAttributeError

instance (p ~ (->), Applicative f) => AsMethodMethodAccessFlagsError p f (ClassFileError c) where
  _MethodMethodAccessFlagsError =
    _ClassFileMethodsError . _MethodMethodAccessFlagsError

instance (p ~ (->), Applicative f) => AsMethodAccessFlagsUnexpectedEof p f (ClassFileError c) where
  _MethodAccessFlagsUnexpectedEof =
    _ClassFileMethodsError . _MethodAccessFlagsUnexpectedEof

class AsClassFileAttributesError p f s where
  _ClassFileAttributesError :: 
    Optic' p f s AttributesError

instance (Choice p, Applicative f) => AsClassFileAttributesError p f (ClassFileError c) where
  _ClassFileAttributesError =
    prism'
      ClassFileAttributesError
      (\e -> case e of
               ClassFileAttributesError r -> Just r
               _ -> Nothing)

instance (p ~ (->), Applicative f) => AsAttributesInfoUnexpectedEof p f (ClassFileError c) where
  _AttributesInfoUnexpectedEof =
    _ClassFileAttributesError . _AttributesInfoUnexpectedEof

instance (p ~ (->), Applicative f) => AsAttributesAttributeError p f (ClassFileError c) where
  _AttributesAttributeError =
    _ClassFileAttributesError . _AttributesAttributeError

instance (p ~ (->), Applicative f) => AsAttributeNameIndexUnexpectedEof p f (ClassFileError c) where
  _AttributeNameIndexUnexpectedEof =
    _ClassFileAttributesError . _AttributeNameIndexUnexpectedEof

instance (p ~ (->), Applicative f) => AsAttributeLengthUnexpectedEof p f (ClassFileError c) where
  _AttributeLengthUnexpectedEof =
    _ClassFileAttributesError . _AttributeLengthUnexpectedEof

instance (p ~ (->), Applicative f) => AsAttributeUnexpectedEof p f (ClassFileError c) where
  _AttributeUnexpectedEof =
    _ClassFileAttributesError . _AttributeUnexpectedEof

class AsClassFileUnexpectedInputOnStream p f s where
  _ClassFileUnexpectedInputOnStream :: 
    Optic' p f s ()

instance (Choice p, Applicative f) => AsClassFileUnexpectedInputOnStream p f (ClassFileError c) where
  _ClassFileUnexpectedInputOnStream =
    prism'
      (\() -> ClassFileUnexpectedInputOnStream)
      (\e -> case e of
               ClassFileUnexpectedInputOnStream -> Just ()
               _ -> Nothing)

classFileUnexpectedInputOnStream ::
  AsClassFileUnexpectedInputOnStream Tagged Identity t =>
  t
classFileUnexpectedInputOnStream =
  _ClassFileUnexpectedInputOnStream # ()

classFile ::
  (AsEmpty (c Word8), AsEmpty (t Char),
    AsEmpty (f (Attribute a1)), AsEmpty (a Word8),
    AsEmpty (m (Attribute a2)), AsEmpty (a3 Word8), AsEmpty (a4 Word8),
    AsEmpty (c1 (ConstantPoolInfo p)), AsEmpty (i Word16),
    AsEmpty (s1 (Field a5 f1)), AsEmpty (t1 (Method m1 b)),
    AsEmpty (u (Attribute d)), Cons (c Word8) (c Word8) Word8 Word8,
    Cons (t Char) (t Char) Char Char,
    Cons
      (f (Attribute a1)) (f (Attribute a1)) (Attribute a) (Attribute a),
    Cons (a Word8) (a Word8) Word8 Word8,
    Cons
      (m (Attribute a2))
      (m (Attribute a2))
      (Attribute a3)
      (Attribute a3),
    Cons (a3 Word8) (a3 Word8) Word8 Word8,
    Cons (a4 Word8) (a4 Word8) Word8 Word8,
    Cons
      (c1 (ConstantPoolInfo p))
      (c1 (ConstantPoolInfo p))
      (ConstantPoolInfo t)
      (ConstantPoolInfo t),
    Cons (i Word16) (i Word16) Word16 Word16,
    Cons
      (s1 (Field a5 f1)) (s1 (Field a5 f1)) (Field a1 f) (Field a1 f),
    Cons
      (t1 (Method m1 b)) (t1 (Method m1 b)) (Method m a2) (Method m a2),
    Cons
      (u (Attribute d)) (u (Attribute d)) (Attribute a4) (Attribute a4),
    AsClassFileCafebabeError Tagged Identity (s c),
    AsClassFileVersionError Tagged Identity (s c),
    AsClassFileConstantPoolError Tagged Identity s,
    AsClassFileThisAccessFlagsError Tagged Identity (s c),
    AsClassFileThisClassError Tagged Identity (s c),
    AsClassFileSuperClassError Tagged Identity (s c),
    AsClassFileInterfacesError Tagged Identity (s c),
    AsClassFileFieldsError Tagged Identity (s c),
    AsClassFileMethodsError Tagged Identity (s c),
    AsClassFileAttributesError Tagged Identity (s c),
    AsClassFileUnexpectedInputOnStream Tagged Identity (s c)) =>
  Get (s c) (ClassFile p c1 i a5 f1 s1 m1 b t1 d u)
classFile =
  do (_ClassFileCafebabeError #) !!- cafebabe
     v <- (_ClassFileVersionError #) !!- version
     p <- (_ClassFileConstantPoolError #) !!- constantPool
     a <- (_ClassFileThisAccessFlagsError #) !!- thisAccessFlags
     t <- (_ClassFileThisClassError #) !!- thisClass
     s <- (_ClassFileSuperClassError #) !!- superClass
     i <- (_ClassFileInterfacesError #) !!- interfaces
     f <- (_ClassFileFieldsError #) !!- fields
     m <- (_ClassFileMethodsError #) !!- methods
     b <- (_ClassFileAttributesError #) !!- attributes
     e <- isEmpty
     if e
       then
         return (ClassFile v p a t s i f m b)
       else
         failGet classFileUnexpectedInputOnStream
