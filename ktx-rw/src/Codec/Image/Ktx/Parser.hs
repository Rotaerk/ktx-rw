{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}


module Codec.Image.Ktx.Parser (
  fileIdentifier,
  relativeEndianness,
  RelativeEndianness (..),
  withRelativeEndianness,
  header,
  keyValueData,
  anyWord32Endian,
  mipLevel,
  textureData,
  module Data.Attoparsec.ByteString
) where

import Codec.Image.Ktx.Types

import Prelude hiding (take)
import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans.Class
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.Functor
import qualified Data.Functor.Trans.Tagged as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Singletons.TH
import Data.Word
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (Storable, peekByteOff)
import System.IO.Unsafe (unsafeDupablePerformIO)

fileIdentifier :: Parser ()
fileIdentifier = void . string . BS.pack $ [ 0xAB, 0x4B, 0x54, 0x58, 0x20, 0x31, 0x31, 0xBB, 0x0D, 0x0A, 0x1A, 0x0A ]

unsafeFromByteString :: Storable a => ByteString -> a
unsafeFromByteString bs =
  unsafeDupablePerformIO $ do
    let (fp, offset, _) = BSI.toForeignPtr bs
    withForeignPtr fp $ \p -> peekByteOff p offset

anyWord16 :: Parser Word16
anyWord16 = unsafeFromByteString <$> take 2

word16 :: Word16 -> Parser Word16
word16 w = mfilter (== w) anyWord16

anyWord32 :: Parser Word32
anyWord32 = unsafeFromByteString <$> take 4

word32 :: Word32 -> Parser Word32
word32 w = mfilter (== w) anyWord32

genSingletons [''RelativeEndianness]

relativeEndianness :: Parser RelativeEndianness
relativeEndianness = SameEndian <$ word32 0x04030201 <|> FlipEndian <$ word32 0x01020304

type EndianParser (e :: RelativeEndianness) a = T.TaggedT e Parser a

class ParseEndian (e :: RelativeEndianness) where
  anyWord16Endian :: EndianParser e Word16
  anyWord32Endian :: EndianParser e Word32
  takeWordsEndian :: Int {- Word size -} -> Int {- Word count -} -> EndianParser e ByteString

instance ParseEndian 'SameEndian where
  anyWord16Endian = T.tagT anyWord16
  anyWord32Endian = T.tagT anyWord32

  takeWordsEndian s n = T.tagT $ take (s * n)

instance ParseEndian 'FlipEndian where
  anyWord16Endian = T.tagT $ byteSwap16 <$> anyWord16
  anyWord32Endian = T.tagT $ byteSwap32 <$> anyWord32

  takeWordsEndian 1 n = T.tagT $ take n
  takeWordsEndian s n = T.tagT $ BS.concat <$> count n (BS.reverse <$> take s)

withRelativeEndianness :: RelativeEndianness -> (forall e. ParseEndian e => EndianParser e a) -> Parser a
withRelativeEndianness (toSing -> SomeSing reSing) = untagWith reSing
  where
  untagWith :: Sing e' -> (ParseEndian e' => EndianParser e' a) -> Parser a
  untagWith SSameEndian = T.untagT
  untagWith SFlipEndian = T.untagT

header :: ParseEndian e => EndianParser e Header
header =
  replicateM 12 anyWord32Endian <&>
  \[
    glType,
    glTypeSize,
    glFormat,
    glInternalFormat,
    glBaseInternalFormat,
    pixelWidth,
    pixelHeight,
    pixelDepth,
    numberOfArrayElements,
    numberOfFaces,
    numberOfMipmapLevels,
    bytesOfKeyValueData
  ] ->
    Header { 
      header'glType = glType,
      header'glTypeSize = glTypeSize,
      header'glFormat = glFormat,
      header'glInternalFormat = glInternalFormat,
      header'glBaseInternalFormat = glBaseInternalFormat,
      header'pixelWidth = pixelWidth,
      header'pixelHeight = pixelHeight,
      header'pixelDepth = pixelDepth,
      header'numberOfArrayElements = numberOfArrayElements,
      header'numberOfFaces = numberOfFaces,
      header'numberOfMipmapLevels = numberOfMipmapLevels,
      header'bytesOfKeyValueData = bytesOfKeyValueData
    }

pad :: Integral n => n -> n -> n
pad size align = let m = align - 1 in m - ((size + m) `mod` align)

keyValueData :: ParseEndian e => Int -> EndianParser e [(ByteString, ByteString)]
keyValueData =
  unfoldrM $ \remaining ->
    if remaining <= 0 then -- if remaining is ever < 0, it's technically an invalid KTX file, but handling just in case...
      return Nothing
    else do
      keyAndValueByteSize <- fromIntegral <$> anyWord32Endian
      let paddingSize = pad keyAndValueByteSize 4
      pair <- lift $ do
        key <- takeThrough (== 0)
        value <- take (keyAndValueByteSize - BS.length key)
        void $ take paddingSize
        return (key, value)
      return . Just $ (pair, remaining - 4 - keyAndValueByteSize - paddingSize) -- The 4 is from the keyAndValueByteSize word32 itself.

mipLevel :: ParseEndian e => Header -> EndianParser e [ByteString]
mipLevel header = do
  imageSize <- anyWord32Endian
  replicateM (if isCubeMap header && not (isArray header) then 6 else 1) $ do
    pixelData <- takeWordsEndian (fromIntegral glTypeSize) (fromIntegral $ imageSize `quot` glTypeSize)
    void $ lift $ take (fromIntegral $ pad imageSize 4)
    return pixelData
  where
    glTypeSize = header'glTypeSize header

textureData :: ParseEndian e => Header -> EndianParser e [[ByteString]]
textureData header = replicateM (fromIntegral . oneIfPalettedFormat header . zeroToOne $ header'numberOfMipmapLevels header) (mipLevel header)

isCubeMap :: Header -> Bool
isCubeMap = (6 ==) . header'numberOfFaces

isArray :: Header -> Bool
isArray = (0 <) . header'numberOfArrayElements

isCompressed :: Header -> Bool
isCompressed = (0 ==) . header'glType

hasPalettedInternalFormat :: Header -> Bool
hasPalettedInternalFormat = (`Set.member` palettedFormats) . header'glInternalFormat
  where
    palettedFormats =
      Set.fromList [
        0x8B90, --GL_PALETTE4_RGB8_OES
        0x8B91, --GL_PALETTE4_RGBA8_OES
        0x8B92, --GL_PALETTE4_R5_G6_B5_OES
        0x8B93, --GL_PALETTE4_RGBA4_OES
        0x8B94, --GL_PALETTE4_RGB5_A1_OES
        0x8B95, --GL_PALETTE8_RGB8_OES
        0x8B96, --GL_PALETTE8_RGBA8_OES
        0x8B97, --GL_PALETTE8_R5_G6_B5_OES
        0x8B98, --GL_PALETTE8_RGBA4_OES
        0x8B99  --GL_PALETTE8_RGB5_A1_OES
      ]

zeroToOne :: Integral n => n -> n
zeroToOne 0 = 1
zeroToOne x = x

oneIfPalettedFormat :: Integral n => Header -> n -> n
oneIfPalettedFormat h = if hasPalettedInternalFormat h then const 1 else id

takeThrough :: (Word8 -> Bool) -> Parser ByteString
takeThrough p =
  scan False $ \case
    False -> Just . p
    True -> const Nothing

