{-# LANGUAGE TypeFamilies #-}

module Control.Monad.BinaryRead.Class where

import Control.Exception
import Control.Monad
import Control.Monad.Catch
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.ByteString.Local
import Data.Word
import Foreign.Ptr

class MonadThrow m => MonadBinaryRead m where
  type BinaryReadBookmark m
  dataSize :: m Integer
  createBookmark :: m (BinaryReadBookmark m)
  goToBookmark :: BinaryReadBookmark m -> m ()
  seekRel :: Integer -> m ()
  tryReadToPtr :: Ptr a -> Int -> m Int
  tryReadBS :: Int -> m ByteString

data BinaryReadException = BinaryReadException String deriving (Eq, Show, Read)

instance Exception BinaryReadException where
  displayException (BinaryReadException message) = message

throwBinaryReadException :: MonadThrow m => String -> m a
throwBinaryReadException = throwM . BinaryReadException

readToPtr :: MonadBinaryRead m => Ptr a -> Int -> m ()
readToPtr ptr numBytesToRead = tryReadToPtr ptr numBytesToRead >>= checkNumBytesObtained numBytesToRead

readBS :: MonadBinaryRead m => Int -> m ByteString
readBS numBytesToRead = do
  bs <- readBS numBytesToRead
  checkNumBytesObtained numBytesToRead (BS.length bs)
  return bs

checkNumBytesObtained :: MonadThrow m => Int -> Int -> m ()
checkNumBytesObtained numBytesToRead numBytesObtained =
  assert (numBytesObtained <= numBytesToRead) $
  when (numBytesObtained < numBytesToRead) $ throwBinaryReadException "Unexpected EOF."

readWord32 :: MonadBinaryRead m => m Word32
readWord32 = unsafeFromByteString <$> readBS 4
