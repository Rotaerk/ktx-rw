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
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Indexed
import Control.Monad.Loops
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.Functor
import Data.Functor.Indexed
import Data.Kind
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import GHC.TypeLits
import System.IO
import System.IO.Unsafe

import Prelude hiding (take)

type family Fst (xy :: (x,y)) :: x where Fst '(x,y) = x

data Position =
  Position'Identifier |
  Position'Header |
  Position'Metadata |
  Position'TextureData |
  Position'End

data ReadException = ReadException String deriving (Eq, Show, Read)

instance Exception ReadException where
  displayException (ReadException message) = "Invalid KTX file: " ++ message

throwReadException :: MonadThrow m => String -> m a
throwReadException = throwM . ReadException

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

class MonadThrow m => MonadBinaryRead m where
  type BinaryReadBookmark m
  brDataSize :: m Integer
  brCreateBookmark :: m (BinaryReadBookmark m)
  brGoToBookmark :: BinaryReadBookmark m -> m ()
  brSeekRel :: Integer -> m ()
  brTryReadToBuf :: Ptr a -> Int -> m Int
  brTryReadBS :: Int -> m ByteString

unsafeFromByteString :: Storable a => ByteString -> a
unsafeFromByteString bs =
  unsafeDupablePerformIO $ do
    let (fp, offset, _) = BSI.toForeignPtr bs
    withForeignPtr fp $ \p -> peekByteOff p offset

brReadBS :: MonadBinaryRead m => Int -> m ByteString
brReadBS size = do
  bs <- brTryReadBS size
  when (BS.length bs /= size) $ throwReadException "Unexpected EOF."
  return bs

brReadWord32 :: MonadBinaryRead m => RelativeEndianness -> m Word32
brReadWord32 re = bsToWord32With re <$> brReadBS 4

bsToWord32With :: RelativeEndianness -> ByteString -> Word32
bsToWord32With SameEndian = unsafeFromByteString
bsToWord32With FlipEndian = byteSwap32 . unsafeFromByteString

newtype KtxReadTBookmark (m :: * -> *) (p :: Position) = KtxReadTBookmark (BinaryReadBookmark m)

createBookmark :: MonadBinaryRead m => KtxReadT m '(h, p) '(h, p) (KtxReadTBookmark m p)
createBookmark = KtxReadTBookmark <<$>> liftToKtxReadT brCreateBookmark

goToBookmark :: MonadBinaryRead m => KtxReadTBookmark m p' -> KtxReadT m '(h, p) '(h, p') ()
goToBookmark (KtxReadTBookmark brb) = liftToKtxReadT $ brGoToBookmark brb

readAndCheckIdentifier :: MonadBinaryRead m => KtxReadT m '(h, Position'Identifier) '(h, Position'Header) ()
readAndCheckIdentifier = liftToKtxReadT $ do
  bs <- brTryReadBS 12
  when (bs /= identifier) $ throwReadException "KTX file identifier not found."
  return ()

  where
    identifier = BS.pack $ [ 0xAB, 0x4B, 0x54, 0x58, 0x20, 0x31, 0x31, 0xBB, 0x0D, 0x0A, 0x1A, 0x0A ]

readHeader :: MonadBinaryRead m => KtxReadT m '(h, Position'Header) '(Header, Position'Metadata) Header
readHeader = KtxReadT $ const $ do
  re <-
    brReadWord32 SameEndian >>= \case
      0x04030201 -> return SameEndian
      0x01020304 -> return FlipEndian
      _ -> throwReadException "Invalid endianness indicator."
  header <-
    replicateM 12 (brReadWord32 re) <&>
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
        header'relativeEndianness = re,
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
  return (header, header)

readHeader_ :: MonadBinaryRead m => KtxReadT m '(h, Position'Header) '(Header, Position'Metadata) ()
readHeader_ = const () <<$>> readHeader

getHeader :: Applicative m => KtxReadWithHeaderT m p p' Header
getHeader = KtxReadT $ \h -> pure (h, h)

skipMetadata :: MonadBinaryRead m => KtxReadWithHeaderT m Position'Metadata Position'TextureData ()
skipMetadata = header'bytesOfKeyValueData <<$>> getHeader >>>= liftToKtxReadT . brSeekRel . fromIntegral

readMetadata :: MonadBinaryRead m => KtxReadWithHeaderT m Position'Metadata Position'TextureData [(ByteString, ByteString)]
readMetadata =
  getHeader >>>= \header ->
  let
    size = fromIntegral . header'bytesOfKeyValueData $ header
    re = header'relativeEndianness header
  in
    liftToKtxReadT $ parse (parseMetadata re size) <$> brReadBS size >>= \case
      Done (BS.length -> 0) result -> return result
      Done (BS.length -> len) _ -> throwReadException $ "Metadata parser did not fully consume input.  This shouldn't happen.  Implementation broken."
      Fail rest contexts message -> throwReadException $ "Malformed metadata."
      Partial _ -> throwReadException $ "Metadata parser expecting more input.  This shouldn't happen.  Implementation broken."

pad :: Integral n => n -> n -> n
pad size align = let m = align - 1 in m - ((size + m) `mod` align)

parseAnyWord32With :: RelativeEndianness -> Parser Word32
parseAnyWord32With re = bsToWord32With re <$> take 4

takeThrough :: (Word8 -> Bool) -> Parser ByteString
takeThrough p =
  scan False $ \case
    False -> Just . p
    True -> const Nothing

parseMetadata :: RelativeEndianness -> Int -> Parser [(ByteString, ByteString)]
parseMetadata re =
  unfoldrM $ \remaining ->
    if remaining <= 0 then -- if remaining is ever < 0, it's technically an invalid KTX file, but handling just in case...
      return Nothing
    else do
      keyAndValueByteSize <- fromIntegral <$> parseAnyWord32
      key <- takeThrough (== 0)
      value <- take (keyAndValueByteSize - BS.length key)
      let paddingSize = pad keyAndValueByteSize 4
      void $ take paddingSize
      return . Just $ ((key, value), remaining - 4 - keyAndValueByteSize - paddingSize) -- The 4 is from the keyAndValueByteSize word32 itself.
  where
    parseAnyWord32 = parseAnyWord32With re

type Offset = Int
type Size = Int

type SimpleBufferRegion = (Size, Offset)
type NonArrayCubeMapBufferRegion = (Size, Offset, Offset, Offset, Offset, Offset, Offset)

data BufferRegions =
  SimpleBufferRegions [SimpleBufferRegion] |
  NonArrayCubeMapBufferRegions [NonArrayCubeMapBufferRegion]

getPixelDataSize :: MonadBinaryRead m => KtxReadWithHeaderT m p p Integer
getPixelDataSize =
  getHeader >>>= \header ->
  liftToKtxReadT $ do
    totalDataSize <- brDataSize
    let
      metadataSize = fromIntegral . header'bytesOfKeyValueData $ header
      numMipLevels = fromIntegral . header'numberOfMipmapLevels $ header
    return $ totalDataSize - identifierSize - headerSize - metadataSize - numMipLevels * imageSizeFieldSize

  where
    word32Size = 4
    identifierSize = 12
    headerSize = 13 * word32Size
    imageSizeFieldSize = word32Size

readAllDataInto :: (MonadIO m, MonadBinaryRead m) => Ptr Word8 -> KtxReadWithHeaderT m Position'TextureData Position'End BufferRegions
readAllDataInto ptr =
  getHeader >>>= \header ->
  let
    numMipmapLevels = if hasPalettedInternalFormat header then 1 else fromIntegral . replace 0 1 $ header'numberOfMipmapLevels header
    wordSize = fromIntegral $ header'glTypeSize header
    re = header'relativeEndianness header
    readToBuffer' = readToBuffer re
  in
    liftToKtxReadT $ do
      imageSize <- fromIntegral <$> brReadWord32 re
      evalBufferWriteTOn ptr wordSize $
        if isCubeMap header && not (isArray header) then
          fmap NonArrayCubeMapBufferRegions . replicateM numMipmapLevels $ do
            [o1, o2, o3, o4, o5, o6] <- replicateM 6 $ readToBuffer' (alignTo 4 imageSize)
            return (imageSize, o1, o2, o3, o4, o5, o6)
        else
          fmap SimpleBufferRegions . replicateM numMipmapLevels $ do
            offset <- readToBuffer' (alignTo 4 imageSize)
            return (imageSize, offset)

type BufferWriteEnv = (Ptr Word8, Int)
type BufferWriteT m = StateT Offset (ReaderT BufferWriteEnv m)

evalBufferWriteTOn :: Monad m => Ptr Word8 -> Int -> BufferWriteT m a -> m a
evalBufferWriteTOn bufferPtr wordSize bf = runReaderT (evalStateT bf 0) (bufferPtr, wordSize)

writeBufferWith :: Monad m => (Ptr Word8 -> Int -> m Size) -> BufferWriteT m (Offset, Size)
writeBufferWith write = do
  (bufferPtr, wordSize) <- lift ask
  offset <- get
  size <- lift . lift $ write (bufferPtr `plusPtr` offset) wordSize
  put (offset + size)
  return (offset, size)

readToBuffer :: (MonadIO m, MonadBinaryRead m) => RelativeEndianness -> Size -> BufferWriteT m Offset
readToBuffer re size = do
  fmap fst . writeBufferWith $ \offsetPtr wordSize -> do
    numBytesRead <- brTryReadToBuf offsetPtr size
    when (numBytesRead < size) $ throwReadException "Unexpected EOF."
    when (re == FlipEndian) $ liftIO $ byteSwapWordsInPlace wordSize offsetPtr (size `quot` wordSize)
    return size

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

alignTo :: Integral n => n -> n -> n
alignTo b n =
  case n `rem` b of
    0 -> n
    x -> n + b - x

replace :: Eq a => a -> a -> a -> a
replace match replacement value | value == match = replacement
replace _ _ value = value
