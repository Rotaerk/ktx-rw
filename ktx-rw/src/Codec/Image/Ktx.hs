{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Proxy
import Data.Reflection
import Data.Word

parseFileIdentifier :: Parser ()
parseFileIdentifier = void . AP.string . BS.pack $ [ 0xAB, 0x4B, 0x54, 0x58, 0x20, 0x31, 0x31, 0xBB, 0x0D, 0x0A, 0x1A, 0x0A ]

type TaggedParser e a = T.TaggedT e Parser a

data Endianness = BigEndian | LittleEndian deriving Show

parseEndianness :: Parser Endianness
parseEndianness =
  BigEndian <$ AP.string (BS.pack [4,3,2,1]) <|>
  LittleEndian <$ AP.string (BS.pack [1,2,3,4])

type Endian e = Reifies e Endianness

takeBigEndian :: forall e. Endian e => Int -> TaggedParser e ByteString
takeBigEndian = T.tagT . toBigEndian . AP.take
  where
    toBigEndian =
      case reflect (Proxy :: Proxy e) of
        BigEndian -> id
        LittleEndian -> fmap BS.reverse

parseWord32 :: forall e. Endian e => TaggedParser e Word32
parseWord32 = foldl (\n b -> shiftL n 8 .|. b) 0 . fmap fromIntegral . BS.unpack <$> takeBigEndian 4

takeThrough :: (Word8 -> Bool) -> Parser ByteString
takeThrough p = AP.scan True $ \satisfied b -> if satisfied then Nothing else Just (p b)

parseKeyValuePair :: forall e. Endian e => TaggedParser e (ByteString, ByteString)
parseKeyValuePair = do
  keyAndValueByteSize <- fromIntegral <$> parseWord32
  lift $ do
    key <- takeThrough (== 0)
    value <- AP.take (keyAndValueByteSize - BS.length key)
    replicateM_ (3 - ((keyAndValueByteSize + 3) `mod` 4)) AP.anyWord8
    return (key, value)

parseKeyValueData :: forall e. Endian e => Int -> TaggedParser e [(ByteString, ByteString)]
parseKeyValueData =
  unfoldrM $ \remaining ->
    if remaining <= 0 then -- if remaining is ever < 0, it's technically an invalid KTX file, but just in case...
      return Nothing
    else do
      pair@(key, value) <- parseKeyValuePair
      return . Just $ (pair, remaining - BS.length key - BS.length value)

parseKtx :: Parser ktx
parseKtx = do
  parseFileIdentifier
  endianness <- parseEndianness
  reify endianness $ T.proxyT $
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
        return undefined
