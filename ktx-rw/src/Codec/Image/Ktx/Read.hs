{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.Image.Ktx.Read where

import Codec.Image.Ktx.Types
import Control.Exception
import Control.Monad
import Control.Monad.BinaryRead.Class as BR
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Indexed
import Control.Monad.Loops
import Control.Monad.Trans.BufferWriter
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.ByteString.Local
import Data.Functor
import Data.Functor.Indexed
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import System.IO

import Prelude hiding (take)

type family Fst (xy :: (x,y)) :: x where Fst '(x,y) = x

data Position =
  Position'Identifier |
  Position'Header |
  Position'Metadata |
  Position'TextureData |
  Position'End

data KtxReadException = KtxReadException String deriving (Eq, Show, Read)

instance Exception KtxReadException where
  displayException (KtxReadException message) = "Invalid KTX file: " ++ message

throwKtxReadException :: MonadThrow m => String -> m a
throwKtxReadException = throwM . KtxReadException

newtype KtxReadT (m :: * -> *) (s :: (*, Position)) (s' :: (*, Position)) a =
  KtxReadT { runKtxReadT :: Fst s -> m (a, Fst s') }

type KtxReadWithHeaderT m s s' = KtxReadT m '(Header, s) '(Header, s')

liftToKtxReadT :: Monad m => m a -> KtxReadT m '(h, p) '(h, p') a
liftToKtxReadT m = KtxReadT $ \h -> (,h) <$> m

instance Functor m => IxFunctor (KtxReadT m) where
  imap atb ka = KtxReadT $ \h -> runKtxReadT ka h <&> \(a, h') -> (atb a, h')

instance Applicative m => IxPointed (KtxReadT m) where
  ireturn a = KtxReadT $ \h -> pure (a, h)

instance Monad m => IxApplicative (KtxReadT m) where
  iap katb ka = KtxReadT $ \h -> do
    (atb, h') <- runKtxReadT katb h
    (a, h'') <- runKtxReadT ka h'
    return (atb a, h'')

instance Monad m => IxMonad (KtxReadT m) where
  ibind atkb ka = KtxReadT $ \h -> do
    (a, h') <- runKtxReadT ka h
    runKtxReadT (atkb a) h'

newtype KtxReadTBookmark (m :: * -> *) (p :: Position) = KtxReadTBookmark (BinaryReadBookmark m)

createBookmark :: MonadBinaryRead m => KtxReadT m '(h, p) '(h, p) (KtxReadTBookmark m p)
createBookmark = KtxReadTBookmark <<$>> liftToKtxReadT BR.createBookmark

goToBookmark :: MonadBinaryRead m => KtxReadTBookmark m p' -> KtxReadT m '(h, p) '(h, p') ()
goToBookmark (KtxReadTBookmark brb) = liftToKtxReadT $ BR.goToBookmark brb

readAndCheckIdentifier :: MonadBinaryRead m => KtxReadT m '(h, 'Position'Identifier) '(h, 'Position'Header) ()
readAndCheckIdentifier = liftToKtxReadT $ do
  bs <- tryReadBS 12
  when (bs /= identifier) $ throwKtxReadException "KTX file identifier not found."

  where
    identifier = BS.pack $ [ 0xAB, 0x4B, 0x54, 0x58, 0x20, 0x31, 0x31, 0xBB, 0x0D, 0x0A, 0x1A, 0x0A ]

readHeader :: MonadBinaryRead m => KtxReadT m '(h, 'Position'Header) '(Header, 'Position'Metadata) Header
readHeader = KtxReadT $ const $ do
  re <-
    readWord32FromEndianness SameEndian >>= \case
      0x04030201 -> return SameEndian
      0x01020304 -> return FlipEndian
      _ -> throwKtxReadException "Invalid endianness indicator."
  let r = readWord32FromEndianness re
  header <- Header re <$> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r
  return (header, header)

readHeader_ :: MonadBinaryRead m => KtxReadT m '(h, 'Position'Header) '(Header, 'Position'Metadata) ()
readHeader_ = const () <<$>> readHeader

getHeader :: Applicative m => KtxReadWithHeaderT m p p' Header
getHeader = KtxReadT $ \h -> pure (h, h)

skipMetadata :: MonadBinaryRead m => KtxReadWithHeaderT m 'Position'Metadata 'Position'TextureData ()
skipMetadata = header'bytesOfKeyValueData <<$>> getHeader >>>= liftToKtxReadT . seekRel . fromIntegral

readMetadata :: MonadBinaryRead m => KtxReadWithHeaderT m 'Position'Metadata 'Position'TextureData [(ByteString, ByteString)]
readMetadata =
  getHeader >>>= \header ->
  let
    size = fromIntegral . header'bytesOfKeyValueData $ header
    re = header'relativeEndianness header
  in
    liftToKtxReadT $ parse (parseMetadata re size) <$> readBS size >>= \case
      Done (BS.length -> 0) result -> return result
      Done (BS.length -> len) _ -> throwKtxReadException $ "Metadata parser did not fully consume input.  This shouldn't happen.  Implementation broken."
      Fail rest contexts message -> throwKtxReadException $ "Malformed metadata."
      Partial _ -> throwKtxReadException $ "Metadata parser expecting more input.  This shouldn't happen.  Implementation broken."

parseMetadata :: RelativeEndianness -> Int -> Parser [(ByteString, ByteString)]
parseMetadata re =
  unfoldrM $ \remaining ->
    if remaining <= 0 then -- if remaining is ever < 0, it's technically an invalid KTX file, but handling just in case...
      return Nothing
    else do
      keyAndValueByteSize <- fromIntegral <$> parseAnyWord32FromEndianness re
      key <- takeThrough (== 0)
      value <- take (keyAndValueByteSize - BS.length key)
      let paddingSize = padding keyAndValueByteSize 4
      void $ take paddingSize
      return . Just $ ((key, value), remaining - 4 - keyAndValueByteSize - paddingSize) -- The 4 is from the keyAndValueByteSize word32 itself.

getPixelDataSize :: MonadBinaryRead m => KtxReadWithHeaderT m p p Integer
getPixelDataSize =
  getHeader >>>= \h ->
  liftToKtxReadT $ do
    totalDataSize <- dataSize
    let
      metadataSize = fromIntegral $ header'bytesOfKeyValueData h
      numMipLevels = fromIntegral $ header'numberOfMipmapLevels h
    return $ totalDataSize - identifierSize - headerSize - metadataSize - numMipLevels * imageSizeFieldSize

  where
    word32Size = 4
    identifierSize = 12
    headerSize = 13 * word32Size
    imageSizeFieldSize = word32Size

type SimpleBufferRegion = (Size, Offset)
type NonArrayCubeMapBufferRegion = (Size, Offset, Offset, Offset, Offset, Offset, Offset)

data BufferRegions =
  SimpleBufferRegions [SimpleBufferRegion] |
  NonArrayCubeMapBufferRegions [NonArrayCubeMapBufferRegion]

readPixelDataToBuffer :: (MonadIO m, MonadBinaryRead m) => Ptr Word8 -> Size -> KtxReadWithHeaderT m 'Position'TextureData 'Position'End BufferRegions
readPixelDataToBuffer bufferPtr bufferSize =
  getHeader >>>= \h ->
  let
    numMipmapLevels = fromIntegral $ effectiveNumberOfMipmapLevels h
    re = header'relativeEndianness h
    readWord32' = readWord32FromEndianness re
    readWordsToBuffer' = readWordsToBuffer re (fromIntegral $ header'glTypeSize h)
  in
  liftToKtxReadT . evalBufferWriterTOn bufferPtr bufferSize $
  if isNonArrayCubeMap h then
    fmap NonArrayCubeMapBufferRegions . replicateM numMipmapLevels $ do
      imageSize <- liftToBufferWriterT $ fromIntegral <$> readWord32'
      [o1, o2, o3, o4, o5, o6] <- replicateM 6 $ readWordsToBuffer' (alignTo 4 imageSize)
      return (imageSize, o1, o2, o3, o4, o5, o6)
  else
    fmap SimpleBufferRegions . replicateM numMipmapLevels $ do
      imageSize <- liftToBufferWriterT  $ fromIntegral <$> readWord32'
      offset <- readWordsToBuffer' (alignTo 4 imageSize)
      return (imageSize, offset)

readWordsToBuffer ::
  (MonadIO m, MonadBinaryRead m) =>
  RelativeEndianness ->
  Size -> -- bytes per word
  Size -> -- bytes to read
  BufferWriterT m Offset
readWordsToBuffer = \case
  SameEndian -> const $ fmap fst . readToBuffer
  FlipEndian -> \wordSize readSize -> do
    (offset, offsetPtr) <- readToBuffer readSize
    liftIO $ byteSwapWordsInPlace wordSize (castPtr offsetPtr) (readSize `quot` wordSize)
    return offset

byteSwapWordsInPlace :: Size -> Ptr Word8 -> Int -> IO ()
byteSwapWordsInPlace = \case
  1 -> const . const $ return ()
  2 -> mapInPlace byteSwap16 . castPtr
  4 -> mapInPlace byteSwap32 . castPtr
  _ -> undefined

mapInPlace :: Storable a => (a -> a) -> Ptr a -> Int -> IO ()
mapInPlace f ptr elemCount =
  forM_ [0 .. elemCount - 1] $ \offset ->
    f <$> peekElemOff ptr offset >>= pokeElemOff ptr offset

alignTo :: Integral n => n -> n -> n
alignTo b n =
  case n `rem` b of
    0 -> n
    x -> n + b - x

-- The amount of padding required to align data of a given size with a given boundary
padding :: Integral n => n -> n -> n
padding size boundary = let maxPadding = boundary - 1 in maxPadding - ((size + maxPadding) `mod` boundary)

takeThrough :: (Word8 -> Bool) -> Parser ByteString
takeThrough p =
  scan False $ \case
    False -> Just . p
    True -> const Nothing

readWord32FromEndianness :: MonadBinaryRead m => RelativeEndianness -> m Word32
readWord32FromEndianness re = makeSameEndian re <$> readWord32

makeSameEndian :: RelativeEndianness -> Word32 -> Word32
makeSameEndian SameEndian = id
makeSameEndian FlipEndian = byteSwap32

parseAnyWord32 :: Parser Word32
parseAnyWord32 = unsafeFromByteString <$> take 4

parseAnyWord32FromEndianness :: RelativeEndianness -> Parser Word32
parseAnyWord32FromEndianness re = makeSameEndian re <$> parseAnyWord32
