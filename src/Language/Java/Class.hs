{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Language.Java.Class(
  module C
, test
) where

import Language.Java.Class.Attribute as C
import Language.Java.Class.Attributes as C
import Language.Java.Class.Cafebabe as C
import Language.Java.Class.ClassFile as C
import Language.Java.Class.ConstantPool as C
import Language.Java.Class.ConstantPoolInfo as C
import Language.Java.Class.FieldAccessFlags as C
import Language.Java.Class.Field as C
import Language.Java.Class.Fields as C
import Language.Java.Class.Interfaces as C
import Language.Java.Class.MethodAccessFlags as C
import Language.Java.Class.Method as C
import Language.Java.Class.Methods as C
import Language.Java.Class.SuperClass as C
import Language.Java.Class.ThisAccessFlags as C
import Language.Java.Class.ThisClass as C
import Language.Java.Class.Version as C

import Data.Tickle(RunGetResult, Get, runGetFile)
import System.IO(IO)

test ::
  IO (RunGetResult (ClassFileError []) (ClassFile' []))
test =
  runGetFile (classFile :: Get (ClassFileError []) (ClassFile' [])) "/home/tmorris/Desktop/Trademark.class"
