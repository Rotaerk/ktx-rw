{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.FileReader (
  MonadFileReader(..),
  FileReadException(..),
  throwFileReadException,
  readBytesFromFileInto,
  readBytesFromFile,
  readWord32FromFile,
  FileReaderT(),
  buildFileReaderT,
  runFileReaderT
) where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Local
import Data.Word
import Foreign.Ptr
import System.IO

class (MonadIO m, MonadThrow m) => MonadFileReader m where
  getFileSize :: m Integer
  isEndOfFile :: m Bool
  getByteOffsetInFile :: m Integer
  getHandlePosnInFile :: m HandlePosn
  setHandlePosnInFile :: HandlePosn -> m ()
  seekInFile :: SeekMode -> Integer -> m ()
  tryReadBytesFromFileInto :: Ptr a -> Int -> m Int
  tryReadBytesFromFile :: Int -> m ByteString

data FileReadException = FileReadException String deriving (Eq, Show, Read)

instance Exception FileReadException where
  displayException (FileReadException message) = message

throwFileReadException :: MonadThrow m => String -> m a
throwFileReadException = throwM . FileReadException

readBytesFromFileInto :: MonadFileReader m => Ptr a -> Int -> m ()
readBytesFromFileInto buffer count = tryReadBytesFromFileInto buffer count >>= checkNumBytesObtained count

readBytesFromFile :: MonadFileReader m => Int -> m ByteString
readBytesFromFile count = do
  bs <- tryReadBytesFromFile count
  checkNumBytesObtained count (BS.length bs)
  return bs

readWord32FromFile :: MonadFileReader m => m Word32
readWord32FromFile = unsafeFromByteString <$> readBytesFromFile 4

checkNumBytesObtained :: MonadThrow m => Int -> Int -> m ()
checkNumBytesObtained numBytesToRead numBytesObtained =
  assert (numBytesObtained <= numBytesToRead) $
  when (numBytesObtained < numBytesToRead) $ throwFileReadException "Unexpected EOF."

newtype FileReaderT m a = FileReaderT { unFileReaderT :: ReaderT Handle m a }
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadReader Handle, MonadTrans)

buildFileReaderT :: (Handle -> m a) -> FileReaderT m a
buildFileReaderT = FileReaderT . ReaderT

runFileReaderT :: FileReaderT m a -> Handle -> m a
runFileReaderT = runReaderT . unFileReaderT

instance MonadUnliftIO m => MonadUnliftIO (FileReaderT m) where
  withRunInIO = wrappedWithRunInIO FileReaderT unFileReaderT

instance (MonadIO m, MonadThrow m) => MonadFileReader (FileReaderT m) where
  getFileSize = buildFileReaderT $ liftIO . hFileSize
  isEndOfFile = buildFileReaderT $ liftIO . hIsEOF
  getByteOffsetInFile = buildFileReaderT $ liftIO . hTell
  getHandlePosnInFile = buildFileReaderT $ liftIO . hGetPosn
  setHandlePosnInFile = liftIO . hSetPosn
  seekInFile seekMode i = buildFileReaderT $ \h -> liftIO $ hSeek h seekMode i
  tryReadBytesFromFileInto buffer count = buildFileReaderT $ \h -> liftIO $ hGetBuf h buffer count
  tryReadBytesFromFile count = buildFileReaderT $ \h -> liftIO $ BS.hGet h count
