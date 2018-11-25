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
import Control.Monad.Indexed
import Control.Monad.Loops
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
  Position'MipLevel Nat

data ReadException = ReadException String deriving (Eq, Show, Read)

instance Exception ReadException where
  displayException (ReadException message) = "Invalid KTX file: " ++ message

throwReadException :: MonadThrow m => String -> m a
throwReadException = throwM . ReadException

newtype KtxReadT (m :: * -> *) (s :: (*, Position)) (s' :: (*, Position)) a =
  KtxReadT { runKtxReadT :: Fst s -> m (a, Fst s') }

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

getHeader :: Applicative m => KtxReadT m '(Header, p) '(Header, p') Header
getHeader = KtxReadT $ \h -> pure (h, h)

skipMetadata :: MonadBinaryRead m => KtxReadT m '(Header, Position'Metadata) '(Header, Position'MipLevel 0) ()
skipMetadata = header'bytesOfKeyValueData <<$>> getHeader >>>= liftToKtxReadT . brSeekRel . fromIntegral

readMetadata :: MonadBinaryRead m => KtxReadT m '(Header, Position'Metadata) '(Header, Position'MipLevel 0) [(ByteString, ByteString)]
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

readMipLevel :: MonadBinaryRead m => KtxReadT m '(Header, Position'MipLevel n) '(Header, Position'MipLevel (n+1)) ()
readMipLevel = undefined -- TODO

createBookmark :: MonadBinaryRead m => KtxReadT m '(h, p) '(h, p) (KtxReadTBookmark m p)
createBookmark = KtxReadTBookmark <<$>> liftToKtxReadT brCreateBookmark

goToBookmark :: MonadBinaryRead m => KtxReadTBookmark m p' -> KtxReadT m '(h, p) '(h, p') ()
goToBookmark (KtxReadTBookmark brb) = liftToKtxReadT $ brGoToBookmark brb
