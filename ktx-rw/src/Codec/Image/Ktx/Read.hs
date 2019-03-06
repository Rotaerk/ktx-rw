{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.Image.Ktx.Read (
  module Codec.Image.Ktx.Types,
  readKtxFile,
  KtxBodyReaderT(),
  buildKtxBodyReaderT,
  runKtxBodyReaderT,
  KtxFileReadException(..),
  throwKtxFileReadException,
  SimpleBufferRegion,
  NonArrayCubeMapBufferRegion,
  BufferRegions(..),
  readAndCheckIdentifier,
  readHeader,
  skipMetadata,
  readMetadata,
  getTextureDataSize,
  readTextureDataIntoBuffer
) where

import Codec.Image.Ktx.Types
import Control.Exception hiding (bracketOnError)
import Control.Monad
import Control.Monad.BufferWriter
import Control.Monad.Catch
import Control.Monad.Fail
import Control.Monad.FileReader
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.Reader
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.ByteString.Local
import Data.Functor
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import System.IO

import Prelude hiding (take)

readKtxFile :: FilePath -> (Size -> IO (Ptr Word8)) -> (Ptr Word8 -> IO ()) -> IO (Header, Metadata, Buffer, BufferRegions)
readKtxFile filePath allocateBuffer freeBuffer =
  withBinaryFile filePath ReadMode . runFileReaderT $
  readAndCheckIdentifier >> readHeader >>= runKtxBodyReaderT (do
    metadata <- readMetadata
    textureDataSize <- getTextureDataSize
    bracketOnError (liftIO $ allocateBuffer textureDataSize) (liftIO . freeBuffer) $ \((,textureDataSize) -> buffer) -> do
      bufferRegions <- evalBufferWriterT readTextureDataIntoBuffer buffer 0
      ask <&> (, metadata, buffer, bufferRegions)
  )

newtype KtxBodyReaderT m a = KtxBodyReaderT { unKtxBodyReaderT :: ReaderT Header m a }
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadReader Header)

buildKtxBodyReaderT :: (Header -> m a) -> KtxBodyReaderT m a
buildKtxBodyReaderT = KtxBodyReaderT . ReaderT

runKtxBodyReaderT :: KtxBodyReaderT m a -> Header -> m a
runKtxBodyReaderT = runReaderT . unKtxBodyReaderT

instance MonadTrans KtxBodyReaderT where
  lift = KtxBodyReaderT . lift

instance MonadFileReader m => MonadFileReader (KtxBodyReaderT m) where
  getFileSize = lift getFileSize
  isEndOfFile = lift isEndOfFile
  getByteOffsetInFile = lift getByteOffsetInFile
  getHandlePosnInFile = lift getHandlePosnInFile
  setHandlePosnInFile = lift . setHandlePosnInFile
  seekInFile seekMode i = lift $ seekInFile seekMode i
  tryReadBytesFromFileInto buffer n = lift $ tryReadBytesFromFileInto buffer n
  tryReadBytesFromFile = lift . tryReadBytesFromFile

newtype KtxFileReadException = KtxFileReadException String deriving (Eq, Show, Read)

instance Exception KtxFileReadException where
  displayException (KtxFileReadException message) = "Invalid KTX file: " ++ message

throwKtxFileReadException :: MonadThrow m => String -> m a
throwKtxFileReadException = throwM . KtxFileReadException

type SimpleBufferRegion = (Size, Offset)
type NonArrayCubeMapBufferRegion = (Size, Offset, Offset, Offset, Offset, Offset, Offset)

data BufferRegions =
  SimpleBufferRegions [SimpleBufferRegion] |
  NonArrayCubeMapBufferRegions [NonArrayCubeMapBufferRegion]

readAndCheckIdentifier :: MonadFileReader m => m ()
readAndCheckIdentifier = do
  bs <- tryReadBytesFromFile 12
  when (bs /= identifier) $ throwKtxFileReadException "KTX file identifier not found."

  where
    identifier = BS.pack $ [ 0xAB, 0x4B, 0x54, 0x58, 0x20, 0x31, 0x31, 0xBB, 0x0D, 0x0A, 0x1A, 0x0A ]

readHeader :: MonadFileReader m => m Header
readHeader = do
  re <-
    readWord32FromFileWithEndianness SameEndian >>= \case
      0x04030201 -> return SameEndian
      0x01020304 -> return FlipEndian
      _ -> throwKtxFileReadException "Invalid endianness indicator."
  let r = readWord32FromFileWithEndianness re
  Header re <$> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r <*> r

skipMetadata :: MonadFileReader m => KtxBodyReaderT m ()
skipMetadata = buildKtxBodyReaderT $ seekInFile RelativeSeek . fromIntegral . header'bytesOfKeyValueData

readMetadata :: MonadFileReader m => KtxBodyReaderT m Metadata
readMetadata = buildKtxBodyReaderT $ \h ->
  let
    size = fromIntegral . header'bytesOfKeyValueData $ h
    re = header'relativeEndianness h
  in
  parse (parseMetadata re size) <$> readBytesFromFile size >>= \case
    Done (BS.length -> 0) result -> return result
    Done _ _ -> throwKtxFileReadException $ "Metadata parser did not fully consume input.  This shouldn't happen.  Implementation broken."
    Fail _ _ _ -> throwKtxFileReadException $ "Malformed metadata."
    Partial _ -> throwKtxFileReadException $ "Metadata parser expecting more input.  This shouldn't happen.  Implementation broken."

getTextureDataSize :: MonadFileReader m => KtxBodyReaderT m Size
getTextureDataSize = buildKtxBodyReaderT $ \h ->
  let
    metadataSize = fromIntegral . header'bytesOfKeyValueData $ h
    numMipLevels = fromIntegral . header'numberOfMipmapLevels $ h
  in
  getFileSize <&> subtract (identifierSize + headerSize + metadataSize + numMipLevels * imageSizeFieldSize) . fromIntegral
  where
    word32Size = 4
    identifierSize = 12
    headerSize = 13 * word32Size
    imageSizeFieldSize = word32Size

readTextureDataIntoBuffer :: MonadFileReader m => BufferWriterT (KtxBodyReaderT m) BufferRegions
readTextureDataIntoBuffer = do
  h <- lift ask
  let
    numMipmapLevels = fromIntegral $ effectiveNumberOfMipmapLevels h
    re = header'relativeEndianness h
    textureWordSize = fromIntegral $ header'glTypeSize h

    readImageSize :: MonadFileReader m => BufferWriterT (KtxBodyReaderT m) Size
    readImageSize = fromIntegral <$> lift (readWord32FromFileWithEndianness re)

    readImageDataIntoBuffer :: MonadFileReader m => Size -> BufferWriterT (KtxBodyReaderT m) Offset
    readImageDataIntoBuffer = readWordsFromFileIntoBuffer re textureWordSize . (`div` textureWordSize) . alignTo 4

  if isNonArrayCubeMap h then
    fmap NonArrayCubeMapBufferRegions . replicateM numMipmapLevels $ do
      imageSize <- readImageSize
      let r = readImageDataIntoBuffer imageSize
      (imageSize,,,,,,) <$> r <*> r <*> r <*> r <*> r <*> r
  else
    fmap SimpleBufferRegions . replicateM numMipmapLevels $ do
      imageSize <- readImageSize
      (imageSize,) <$> readImageDataIntoBuffer imageSize

readBytesFromFileIntoBuffer :: MonadFileReader m => Int -> BufferWriterT m (Ptr Word8, Offset)
readBytesFromFileIntoBuffer numBytesToRead =
  writeToBufferWith $ \offsetPtr bufferCapacity -> do
    when (numBytesToRead > bufferCapacity) $ throwBufferWriteException $ "Buffer not large enough for this operation.  Capacity: " ++ show bufferCapacity ++ ", Read Size: " ++ show numBytesToRead
    readBytesFromFileInto offsetPtr numBytesToRead
    return (offsetPtr, numBytesToRead)

readWordsFromFileIntoBuffer :: MonadFileReader m => RelativeEndianness -> Size -> Int -> BufferWriterT m Offset
readWordsFromFileIntoBuffer = \case
  SameEndian -> \wordSize numWordsToRead -> snd <$> readBytesFromFileIntoBuffer (numWordsToRead * wordSize)
  FlipEndian -> \wordSize numWordsToRead -> do
    (offsetPtr, offset) <- readBytesFromFileIntoBuffer (numWordsToRead * wordSize)
    liftIO $ byteSwapWordsInPlace wordSize (castPtr offsetPtr) numWordsToRead
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

readWord32FromFileWithEndianness :: MonadFileReader m => RelativeEndianness -> m Word32
readWord32FromFileWithEndianness re = makeSameEndian re <$> readWord32FromFile

makeSameEndian :: RelativeEndianness -> Word32 -> Word32
makeSameEndian SameEndian = id
makeSameEndian FlipEndian = byteSwap32

parseAnyWord32 :: Parser Word32
parseAnyWord32 = unsafeFromByteString <$> take 4

parseAnyWord32FromEndianness :: RelativeEndianness -> Parser Word32
parseAnyWord32FromEndianness re = makeSameEndian re <$> parseAnyWord32

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
