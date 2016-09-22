{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Language.Java.Class.ClassFile(
  ClassFile(..)
, HasClassFile(..)
, ClassFile'
, ClassFileError(..)
, HasClassFileError(..)
, AsClassFileError(..)
, getClassFile
) where

import Data.Tickle(Get, isEmpty, failGet, (!!-))
import Data.Word(Word8, Word16)
import Language.Java.Class.Attribute(Attribute)
import Language.Java.Class.Attributes(Attributes, AttributesError, getAttributes)
import Language.Java.Class.Method(Method)
import Language.Java.Class.Methods(Methods, MethodsError, getMethods)
import Language.Java.Class.Field(Field)
import Language.Java.Class.Fields(Fields, FieldsError, getFields)
import Language.Java.Class.Interfaces(Interfaces, InterfacesError, getInterfaces)
import Language.Java.Class.SuperClass(SuperClass, SuperClassError, getSuperClass)
import Language.Java.Class.ThisClass(ThisClass, ThisClassError, getThisClass)
import Language.Java.Class.ThisAccessFlags(ThisAccessFlags, ThisAccessFlagsError, getThisAccessFlags)
import Language.Java.Class.ConstantPoolInfo(ConstantPoolInfo, AsConstantPoolInfo)
import Language.Java.Class.ConstantPool(ConstantPool, ConstantPoolError, getConstantPool)
import Language.Java.Class.Cafebabe(CafebabeError, getCafebabe)
import Language.Java.Class.Version(Version, VersionError, getVersion)
import Papa

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

makeClassy ''ClassFile

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

makeClassy ''ClassFileError
makeClassyPrisms ''ClassFileError

getClassFile ::
  (AsClassFileError e c1, AsConstantPoolInfo a p, Cons (c1 Word8) (c1 Word8) Word8 Word8, Cons (u (Attribute d)) (u (Attribute d)) (Attribute a5) (Attribute a5), Cons (t (Method m1 b)) (t (Method m1 b)) (Method m a3) (Method m a3), Cons (s (Field a6 f1)) (s (Field a6 f1)) (Field a1 f) (Field a1 f), Cons (i Word16) (i Word16) Word16 Word16, Cons (c (ConstantPoolInfo p1)) (c (ConstantPoolInfo p1)) a a, Cons (a5 Word8) (a5 Word8) Word8 Word8, Cons (a4 Word8) (a4 Word8) Word8 Word8, Cons (m (Attribute a3)) (m (Attribute a3)) (Attribute a4) (Attribute a4), Cons (a2 Word8) (a2 Word8) Word8 Word8, Cons (f (Attribute a1)) (f (Attribute a1)) (Attribute a2) (Attribute a2), Cons (p Char) (p Char) Char Char, AsEmpty (c1 Word8), AsEmpty (u (Attribute d)), AsEmpty (t (Method m1 b)), AsEmpty (s (Field a6 f1)), AsEmpty (i Word16), AsEmpty (c (ConstantPoolInfo p1)), AsEmpty (a5 Word8), AsEmpty (a4 Word8), AsEmpty (m (Attribute a3)), AsEmpty (a2 Word8), AsEmpty (f (Attribute a1)), AsEmpty (p Char)) =>
  Get e (ClassFile p1 c i a6 f1 s m1 b t d u)
getClassFile =
  do  (_ClassFileCafebabeError #) !!- getCafebabe
      v <- (_ClassFileVersionError #) !!- getVersion
      p <- (_ClassFileConstantPoolError #) !!- getConstantPool
      a <- (_ClassFileThisAccessFlagsError #) !!- getThisAccessFlags
      t <- (_ClassFileThisClassError #) !!- getThisClass
      s <- (_ClassFileSuperClassError #) !!- getSuperClass
      i <- (_ClassFileInterfacesError #) !!- getInterfaces
      f <- (_ClassFileFieldsError #) !!- getFields
      m <- (_ClassFileMethodsError #) !!- getMethods
      b <- (_ClassFileAttributesError #) !!- getAttributes
      e <- isEmpty
      if e
        then
          return (ClassFile v p a t s i f m b)
        else
          failGet (_ClassFileUnexpectedInputOnStream # ())
