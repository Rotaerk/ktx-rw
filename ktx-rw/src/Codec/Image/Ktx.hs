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

module Codec.Image.Ktx where

import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans.Class
import qualified Data.Attoparsec.ByteString as AP
import Data.Attoparsec.ByteString (Parser)
import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Functor.Trans.Tagged as T
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Singletons.TH
import Data.Word

takeThrough :: (Word8 -> Bool) -> Parser ByteString
takeThrough p =
  AP.scan False $ \case
    False -> Just . p
    True -> const Nothing

parseFileIdentifier :: Parser ()
parseFileIdentifier = void . AP.string . BS.pack $ [ 0xAB, 0x4B, 0x54, 0x58, 0x20, 0x31, 0x31, 0xBB, 0x0D, 0x0A, 0x1A, 0x0A ]

data Endianness = BigEndian | LittleEndian deriving (Show, Eq)
genSingletons [''Endianness]

parseEndianness :: Parser Endianness
parseEndianness =
  BigEndian <$ AP.string (BS.pack [4,3,2,1]) <|>
  LittleEndian <$ AP.string (BS.pack [1,2,3,4])

type EndianParser (e :: Endianness) a = T.TaggedT e Parser a

class ParseEndian (e :: Endianness) where
  takeBigEndian :: Int -> EndianParser e ByteString

instance ParseEndian 'BigEndian where
  takeBigEndian = T.tagT . AP.take

instance ParseEndian 'LittleEndian where
  takeBigEndian = T.tagT . fmap BS.reverse . AP.take

withEndianness :: Sing e -> (ParseEndian e => EndianParser e a) -> Parser a
withEndianness SBigEndian = T.untagT
withEndianness SLittleEndian = T.untagT

parseWord32 :: ParseEndian e => EndianParser e Word32
parseWord32 = BS.foldl (\n b -> shiftL n 8 .|. fromIntegral b) 0 <$> takeBigEndian 4

parseKeyValuePair :: ParseEndian e => EndianParser e (ByteString, ByteString)
parseKeyValuePair = do
  keyAndValueByteSize <- fromIntegral <$> parseWord32
  lift $ do
    key <- takeThrough (== 0)
    value <- AP.take (keyAndValueByteSize - BS.length key)
    replicateM_ (3 - ((keyAndValueByteSize + 3) `mod` 4)) AP.anyWord8
    return (key, value)

parseKeyValueData :: ParseEndian e => Int -> EndianParser e [(ByteString, ByteString)]
parseKeyValueData =
  unfoldrM $ \remaining ->
    if remaining <= 0 then -- if remaining is ever < 0, it's technically an invalid KTX file, but just in case...
      return Nothing
    else do
      pair@(key, value) <- parseKeyValuePair
      return . Just $ (pair, remaining - BS.length key - BS.length value)

parseTextureData :: ParseEndian e => Word32 -> Word32 -> Word32 -> Int -> Int -> Int -> Int -> Int -> Int -> EndianParser e [a]
parseTextureData glType glTypeSize glInternalFormat pixelWidth pixelHeight pixelDepth numFaces numArrayElements numMipmapLevels =
  replicateM (oneIfPaletteFormat . zeroToOne $ numMipmapLevels) $ do
    imageSize <- parseWord32
    arrayElements <-
      replicateM (zeroToOne numArrayElements) $
        replicateM (oneIfPaletteFormat numFaces) $ do
          zSlices <-
            replicateM (zeroToOne pixelDepth) $
              replicateM (zeroToOne pixelHeight) $
                replicateM pixelWidth $
                  return undefined
          when isCubeMap $ lift $ replicateM_ undefined AP.anyWord8 -- cube padding
          return undefined
    lift $ replicateM_ undefined AP.anyWord8 -- mip padding
    return undefined

  where
    oneIfPaletteFormat = if Set.member glInternalFormat compressedPalettedTextureInternalFormats then const 1 else id
    isCubeMap = numFaces == 6
    isArray = numArrayElements > 0
    isCompressed = glType == 0

zeroToOne :: Integral n => n -> n
zeroToOne 0 = 1
zeroToOne x = x

parseKtx :: Parser ktx
parseKtx = do
  parseFileIdentifier
  SomeSing endiannessSing <- toSing <$> parseEndianness
  withEndianness endiannessSing $
    replicateM 12 parseWord32 >>=
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
      ] -> do
        keyValueData <- parseKeyValueData (fromIntegral bytesOfKeyValueData)
        let
          actualNumMipmapLevels =
            if numberOfMipmapLevels == 0 || Set.member glInternalFormat compressedPalettedTextureInternalFormats then
              1
            else
              numberOfMipmapLevels
          actualNumArrayElements = if numberOfArrayElements == 0 then 1 else numberOfArrayElements
          actualNumFaces = if numberOfFaces == 0 then 1 else numberOfFaces
        return undefined

pattern GL_PALETTE4_RGB8_OES = 0x8B90
pattern GL_PALETTE4_RGBA8_OES = 0x8B91
pattern GL_PALETTE4_R5_G6_B5_OES = 0x8B92
pattern GL_PALETTE4_RGBA4_OES = 0x8B93
pattern GL_PALETTE4_RGB5_A1_OES = 0x8B94
pattern GL_PALETTE8_RGB8_OES = 0x8B95
pattern GL_PALETTE8_RGBA8_OES = 0x8B96
pattern GL_PALETTE8_R5_G6_B5_OES = 0x8B97
pattern GL_PALETTE8_RGBA4_OES = 0x8B98
pattern GL_PALETTE8_RGB5_A1_OES = 0x8B99

compressedPalettedTextureInternalFormats :: Set Word32
compressedPalettedTextureInternalFormats =
  Set.fromList [
    GL_PALETTE4_RGB8_OES,
    GL_PALETTE4_RGBA8_OES,
    GL_PALETTE4_R5_G6_B5_OES,
    GL_PALETTE4_RGBA4_OES,
    GL_PALETTE4_RGB5_A1_OES,
    GL_PALETTE8_RGB8_OES,
    GL_PALETTE8_RGBA8_OES,
    GL_PALETTE8_R5_G6_B5_OES,
    GL_PALETTE8_RGBA4_OES,
    GL_PALETTE8_RGB5_A1_OES
  ]
