{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Language.Java.Class.ConstantPoolInfo (
  ConstantPoolInfo(..)
, HasConstantPoolInfo(..)
, AsConstantPoolInfo(..)
, ConstantPoolInfoError(..)
, HasConstantPoolInfoError(..)
, AsConstantPoolInfoError(..)
, getConstantPoolInfo
) where

import Control.Replicate(replicateO)
import Data.Bifunctor(bimap)
import Data.Bits(Bits, shift, (.&.))
import Data.Bool(bool, (&&))
import Data.Char(chr)
import Data.Int(Int32, Int64)
import Data.Maybe(fromMaybe)
import Data.Tickle((!-), Get, failGet, word8, word16be, int32be, float32be, int64be, float64be)
import Data.Word(Word8, Word16)
import Papa

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

makeClassy ''ConstantPoolInfo
makeClassyPrisms ''ConstantPoolInfo

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

makeClassy ''ConstantPoolInfoError
makeClassyPrisms ''ConstantPoolInfoError

getConstantPoolInfo ::
  (AsConstantPoolInfoError (() -> t) c1, AsConstantPoolInfoError t c, AsConstantPoolInfo b p, Cons (p Char) (p Char) Char Char, Cons (c1 Word8) (c1 Word8) Word8 Word8, AsEmpty (p Char), AsEmpty (c1 Word8)) =>
  Get (() -> t) b
getConstantPoolInfo =
  getConstantPoolInfo' (\_ -> Nothing)

getConstantPoolInfo' ::
  (AsConstantPoolInfoError (() -> t) c1, AsConstantPoolInfoError t c, AsConstantPoolInfo b p, Cons (c1 Word8) (c1 Word8) Word8 Word8, Cons (p Char) (p Char) Char Char, AsEmpty (c1 Word8), AsEmpty (p Char)) =>
  (Word8 -> Maybe (Get (() -> t) b)) -> Get (() -> t) b
getConstantPoolInfo' f =
  let eset = bimap . return
      two16 t e1 e2 =
        do  c <- e1 !- word16be
            n <- e2 !- word16be
            return (t c n)
  in  do  t <- (_ConstantPoolInfoTagUnexpectedEof #) !- word8
          case t of
            1 ->
              do  l <- (_ConstantPoolInfoUtf8LengthUnexpectedEof #) !- word16be
                  b <- replicateO (\n -> _ConstantPoolInfoUtf8UnexpectedEof # n !- word8) l
                  case getJavaString b of
                    Nothing ->
                      failGet (_ConstantPoolInvalidJavaString # b)
                    Just s ->
                      return (_Utf8 # (l, s)) 
            3 ->
              eset (_ConstantPoolInfoConstantIntegerUnexpectedEof #) (_ConstantInteger #) int32be
            4 ->
              eset (_ConstantPoolInfoConstantFloatUnexpectedEof #) (_ConstantFloat #) float32be
            5 ->
              eset (_ConstantPoolInfoConstantLongUnexpectedEof #) (_ConstantLong #) int64be
            6 ->
              eset (_ConstantPoolInfoConstantDoubleUnexpectedEof #) (_ConstantDouble #) float64be
            7 ->
              eset (_ConstantPoolInfoConstantClassUnexpectedEof #) (_ConstantClass #) word16be
            8 ->
              eset (_ConstantPoolInfoConstantStringUnexpectedEof #) (_ConstantString #) word16be
            9 ->
              two16 (curry (_FieldRef #)) (_ConstantPoolInfoFieldRef1UnexpectedEof #) (_ConstantPoolInfoFieldRef2UnexpectedEof #)
            10 ->
              two16 (curry (_MethodRef #)) (_ConstantPoolInfoMethodRef1UnexpectedEof #) (_ConstantPoolInfoMethodRef2UnexpectedEof #)
            11 ->
              two16 (curry (_InterfaceMethodRef #)) (_ConstantPoolInfoInterfaceMethodRef1UnexpectedEof #) (_ConstantPoolInfoMethodRef2UnexpectedEof #)
            12 ->
              two16 (curry (_NameAndType #)) (_ConstantPoolInfoNameAndType1UnexpectedEof #) (_ConstantPoolInfoNameAndType2UnexpectedEof #)
            _ ->
              fromMaybe (failGet (_ConstantPoolInfoInvalidConstantPoolTag # t)) (f t) 
          
getJavaString ::
  (Integral a, Bind m, Applicative m, AsEmpty b, AsEmpty (m b), Cons s s a a, Cons b b Char Char, Bits a) =>
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
                        in getJavaString rest3 >>= (return . (chr i <|)))
                       ((x .&. 0xF0) == 0xE0 && ((y .&. 0xC0) == 0x80) && ((z .&. 0xC0) == 0x80)))
                 (let i = (fromIntegral x .&. 0x1F) `shift` 6 + (fromIntegral y .&. 0x3F)
                  in getJavaString rest2 >>= (return . (chr i <|)))
                 ((x .&. 0xE0) == 0xC0 && ((y .&. 0xC0) == 0x80)))
           (getJavaString rest >>= (return . (chr (fromIntegral x) <|)))
           ((x .&. 0x80) == 0)
