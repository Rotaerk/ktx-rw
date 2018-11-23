{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.Image.Ktx where
  
import Codec.Image.Ktx.Types
import qualified Codec.Image.Ktx.Parser as P
import Codec.Image.Ktx.Parser (Parser, IResult(..))

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import System.IO

identifierSize :: Int
identifierSize = 12

endiannessSize :: Int
endiannessSize = 4

headerSize :: Int
headerSize = 12 * 4

data ReadException = ReadException String deriving (Eq, Show, Read)

instance Exception ReadException where
  displayException (ReadException message) = "Invalid KTX file: " ++ message

throwReadException :: String -> a
throwReadException = throw . ReadException

parseStrictly :: Parser a -> ByteString -> a
parseStrictly p i =
  case P.parse p i of
    Done (BS.length -> 0) result -> result
    Done bs _ -> throwReadException $ "Did not fully consume input.  Remainder size: " ++ show (BS.length bs)
    Fail rest contexts message -> throwReadException $ "Fail (Rest = '" ++ show rest ++ "', Contexts = " ++ show contexts ++ ", Message = '" ++ message ++ "')"
    Partial _ -> throwReadException $ "Partial parse."

getAndParseStrictly :: IO ByteString -> Parser a -> IO a
getAndParseStrictly getBS p = parseStrictly p <$> getBS

readHeader :: FilePath -> IO Header
readHeader filePath =
  withFile filePath ReadMode $ \f ->
    getAndParseStrictly (BS.hGetSome f $ identifierSize + endiannessSize + headerSize) $ do
      P.fileIdentifier
      re <- P.relativeEndianness
      P.withRelativeEndianness re P.header

newtype MetadataReader meta = MetadataReader (Handle -> Word32 -> RelativeEndianness -> IO meta)

skipMetadata :: MetadataReader ()
skipMetadata = MetadataReader $ \h (fromIntegral -> size) _ -> hSeek h RelativeSeek size

readAllMetadata :: MetadataReader [(ByteString, ByteString)]
readAllMetadata = MetadataReader $ \h (fromIntegral -> size) re ->
  getAndParseStrictly (BS.hGetSome h size) $
    P.withRelativeEndianness re (P.keyValueData size)

type Offset = Int
type Size = Int

data StandardMipLevel = StandardMipLevel Offset Size
data NonArrayCubeMapMipLevel = NonArrayCubeMapMipLevel Offset Offset Offset Offset Offset Offset Size

data BufferRegions = StandardBufferRegions [StandardMipLevel] | NonArrayBufferRegions [NonArrayCubeMapMipLevel]

readKtxFile ::
  FilePath ->
  MetadataReader metadata ->
  (Header -> metadata -> Integer -> (Ptr Word8 -> IO BufferRegions) -> IO r) ->
  IO r
readKtxFile filePath mdr cafb = withBinaryFile filePath ReadMode $ \fileHandle -> readKtxHandle fileHandle mdr cafb

readKtxHandle ::
  Handle ->
  MetadataReader metadata ->
  (Header -> metadata -> Integer -> (Ptr Word8 -> IO BufferRegions) -> IO r) ->
  IO r
readKtxHandle fileHandle (MetadataReader readMetadata) createAndFillBuffer = do
  (re, header) <-
    getAndParseStrictly (BS.hGetSome fileHandle $ identifierSize + endiannessSize + headerSize) $ do
      P.fileIdentifier
      re <- P.relativeEndianness
      (re,) <$> P.withRelativeEndianness re P.header
  let bytesOfKeyValueData = header'bytesOfKeyValueData header
  metadata <- readMetadata fileHandle bytesOfKeyValueData re
  fileSize <- hFileSize fileHandle
  startOfData <- hGetPosn fileHandle
  let
    dataSize = fileSize - fromIntegral identifierSize - fromIntegral endiannessSize - fromIntegral headerSize - fromIntegral bytesOfKeyValueData - fromIntegral (header'numberOfMipmapLevels header * 4)
    numMipmapLevels = fromIntegral . P.oneIfPalettedFormat header . P.zeroToOne $ header'numberOfMipmapLevels header
    readMipLevels = if P.isCubeMap header && not (P.isArray header) then readNonArrayCubeMapMipLevels else readStandardMipLevels
  createAndFillBuffer header metadata dataSize $ \ptr -> do
    hSetPosn startOfData
    runBufferFillOn ptr $ readMipLevels header re fileHandle numMipmapLevels

readStandardMipLevels :: Header -> RelativeEndianness -> Handle-> Int -> BufferFill d BufferRegions
readStandardMipLevels header re h =
  fmap StandardBufferRegions . (
    unfoldrM $ \case
      0 -> return Nothing
      mipLevelsRemaining -> do
        ml <- readStandardMipLevel header re h
        return . Just $ (ml, mipLevelsRemaining - 1)
  )

readStandardMipLevel :: Header -> RelativeEndianness -> Handle -> BufferFill d StandardMipLevel
readStandardMipLevel header re h = do
  imageSize <- liftIO $ fromIntegral <$> getAndParseStrictly (BS.hGetSome h 4) (P.withRelativeEndianness re P.anyWord32Endian)
  offset <- fillBuf header re h (alignTo 4 imageSize)
  return $ StandardMipLevel offset imageSize

readNonArrayCubeMapMipLevels :: Header -> RelativeEndianness -> Handle -> Int -> BufferFill d BufferRegions
readNonArrayCubeMapMipLevels header re h =
  fmap NonArrayBufferRegions . (
    unfoldrM $ \case
      0 -> return Nothing
      mipLevelsRemaining -> do
        ml <- readNonArrayCubeMapMipLevel header re h
        return . Just $ (ml, mipLevelsRemaining - 1)
  )

readNonArrayCubeMapMipLevel :: Header -> RelativeEndianness -> Handle -> BufferFill d NonArrayCubeMapMipLevel
readNonArrayCubeMapMipLevel header re h = do
  imageSize <- liftIO $ fromIntegral <$> getAndParseStrictly (BS.hGetSome h 4) (P.withRelativeEndianness re P.anyWord32Endian)
  [o1, o2, o3, o4, o5, o6] <- replicateM 6 $ fillBuf header re h (alignTo 4 imageSize)
  return $ NonArrayCubeMapMipLevel o1 o2 o3 o4 o5 o6 imageSize

fillBuf :: Header -> RelativeEndianness -> Handle -> Size -> BufferFill d Offset
fillBuf header re h size = do
  offset <- get
  ptr <- lift ask
  liftIO $ do
    numBytesRead <- hGetBuf h (ptr `plusPtr` offset) size
    when (numBytesRead < size) $ throwReadException "Unexpected EOF."
    byteSwapWordsInPlace wordSize ptr (size `quot` wordSize)
  put (offset + size)
  return offset

  where
    wordSize = fromIntegral $ header'glTypeSize header

type BufferFill d = StateT Offset (ReaderT (Ptr d) IO)

runBufferFillOn :: Ptr d -> BufferFill d a -> IO a
runBufferFillOn p bf = runReaderT (evalStateT bf 0) p

alignTo :: Integral n => n -> n -> n
alignTo b n =
  case n `rem` b of
    0 -> n
    x -> n + b - x

byteSwapWordsInPlace :: Int -> Ptr a -> Int -> IO ()
byteSwapWordsInPlace = \case
  1 -> const . const $ return ()
  2 -> mapInPlace byteSwap16 . castPtr
  4 -> mapInPlace byteSwap32 . castPtr
  _ -> undefined

mapInPlace :: Storable a => (a -> a) -> Ptr a -> Int -> IO ()
mapInPlace f ptr count =
  forM_ [0 .. count - 1] $ \offset ->
    f <$> peekElemOff ptr offset >>= pokeElemOff ptr offset
