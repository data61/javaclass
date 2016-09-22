{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Language.Java.Class.ConstantPool(
  ConstantPool(..)
, HasConstantPool(..)
, ConstantPoolError(..)
, HasConstantPoolError(..)
, AsConstantPoolError(..)
, getConstantPool
, getConstantPool'
) where

import Control.Applicative(liftA2)
import Control.Replicate(replicateN)
import Data.Bool(bool, (&&))
import Data.Tickle(Get, word16be, (!-), (!!-))
import Data.Word(Word8, Word16)
import Language.Java.Class.ConstantPoolInfo(ConstantPoolInfo, ConstantPoolInfoError, AsConstantPoolInfo(_ConstantDouble, _ConstantLong), getConstantPoolInfo)
import Papa

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4 4.4. The Constant Pool>
data ConstantPool p c =
  ConstantPool
    Word16
    (c (ConstantPoolInfo p))

makeClassy ''ConstantPool

deriving instance Eq (c (ConstantPoolInfo p)) => Eq (ConstantPool p c)
deriving instance Ord (c (ConstantPoolInfo p)) => Ord (ConstantPool p c)
deriving instance Show (c (ConstantPoolInfo p)) => Show (ConstantPool p c)

data ConstantPoolError c =
  ConstantPoolCountUnexpectedEof
  | ConstantPoolConstantPoolInfoError (ConstantPoolInfoError c)

makeClassy ''ConstantPoolError
makeClassyPrisms ''ConstantPoolError

deriving instance Eq (ConstantPoolInfoError c) => Eq (ConstantPoolError c)
deriving instance Ord (ConstantPoolInfoError c) => Ord (ConstantPoolError c)
deriving instance Show (ConstantPoolInfoError c) => Show (ConstantPoolError c)

getConstantPool ::
  (AsConstantPoolError e c, AsConstantPoolInfo a p, Cons (p Char) (p Char) Char Char, Cons (c Word8) (c Word8) Word8 Word8, Cons (c1 (ConstantPoolInfo p1)) (c1 (ConstantPoolInfo p1)) a a, AsEmpty (p Char), AsEmpty (c Word8), AsEmpty (c1 (ConstantPoolInfo p1))) =>
  Get e (ConstantPool p1 c1)
getConstantPool =
  getConstantPool' getConstantPoolInfo

getConstantPool' ::
  (AsConstantPoolError e c, AsConstantPoolInfo a p, Cons (c1 (ConstantPoolInfo p1)) (c1 (ConstantPoolInfo p1)) a a, AsEmpty (c1 (ConstantPoolInfo p1))) =>
  Get (ConstantPoolInfoError c) a
  -> Get e (ConstantPool p1 c1)
getConstantPool' cpi =
  let jump = bool (subtract 1) id . liftA2 (&&) (isn't _ConstantLong) (isn't _ConstantDouble)
  in  do  constant_pool_count <- (_ConstantPoolCountUnexpectedEof # ()) !- word16be
          pool <- (_ConstantPoolConstantPoolInfoError #) !!- replicateN (\n -> (\i -> (jump i n, i)) <$> cpi) (constant_pool_count - 1)
          return (ConstantPool constant_pool_count pool)
