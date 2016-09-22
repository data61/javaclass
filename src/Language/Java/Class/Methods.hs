{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Language.Java.Class.Methods (
  Methods(..)
, HasMethods(..)
, MethodsError(..)
, HasMethodsError(..)
, AsMethodsError(..)
, getMethods
) where

import Control.Replicate(replicateO)
import Data.Tickle(Get, (!!-), (!-), word16be)
import Data.Word(Word8, Word16)
import Language.Java.Class.Attribute(Attribute)
import Language.Java.Class.Method(Method, MethodError, getMethod)
import Papa

-- |
--
-- <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1 4.1. The ClassFile Structure>
data Methods m a s =
  Methods
    Word16
    (s (Method m a))

makeClassy ''Methods

deriving instance Eq (s (Method m a)) => Eq (Methods m a s)
deriving instance Ord (s (Method m a)) => Ord (Methods m a s)
deriving instance Show (s (Method m a)) => Show (Methods m a s)

data MethodsError =
  MethodsInfoUnexpectedEof
  | MethodsMethodError Word16 MethodError
  deriving (Eq, Ord, Show)

makeClassy ''MethodsError
makeClassyPrisms ''MethodsError

getMethods ::
  (AsMethodsError e, Cons (s (Method m1 a2)) (s (Method m1 a2)) (Method m a1) (Method m a1), Cons (a Word8) (a Word8) Word8 Word8, Cons (m (Attribute a1)) (m (Attribute a1)) (Attribute a) (Attribute a), AsEmpty (s (Method m1 a2)), AsEmpty (a Word8), AsEmpty (m (Attribute a1))) =>
  Get e (Methods m1 a2 s)
getMethods =
  do c <- (_MethodsInfoUnexpectedEof # ()) !- word16be
     i <- (_MethodsMethodError #) !!- replicateO (\n -> ((,) n) !!- getMethod) c
     return (Methods c i)
