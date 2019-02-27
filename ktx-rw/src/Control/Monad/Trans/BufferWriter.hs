{-# LANGUAGE LambdaCase #-}

module Control.Monad.Trans.BufferWriter where

import Control.Exception
import Control.Monad
import Control.Monad.BinaryRead.Class
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Data.Word
import Foreign.Ptr

type Offset = Int
type Size = Int
  
type BufferWriterEnv = (Ptr Word8, Size)
type BufferWriterT m = StateT Offset (ReaderT BufferWriterEnv m)

newtype BufferWriteException = BufferWriteException String deriving (Eq, Show, Read)

instance Exception BufferWriteException where
  displayException (BufferWriteException message) = message

throwBufferWriteException :: MonadThrow m => String -> m a
throwBufferWriteException = throwM . BufferWriteException

evalBufferWriterTOn :: Monad m => Ptr Word8 -> Size -> BufferWriterT m a -> m a
evalBufferWriterTOn bufferPtr size bf = runReaderT (evalStateT bf 0) (bufferPtr, size)

liftToBufferWriterT :: Monad m => m a -> BufferWriterT m a
liftToBufferWriterT = lift . lift

writeBufferWith :: Monad m => (Ptr Word8 -> Size -> m (Size, a)) -> BufferWriterT m (Offset, a)
writeBufferWith write = do
  (bufferPtr, bufferSize) <- lift ask
  offset <- get
  (numBytesWritten, a) <- liftToBufferWriterT $ write (bufferPtr `plusPtr` offset) (bufferSize - offset)
  put (offset + numBytesWritten)
  return (offset, a)

readToBuffer :: (MonadIO m, MonadBinaryRead m) => Size -> BufferWriterT m (Offset, Ptr Word8)
readToBuffer readSize =
  writeBufferWith $ \offsetPtr bufferCapacity -> do
    when (readSize > bufferCapacity) $ throwBufferWriteException "Buffer not large enough for this operation."
    readToPtr offsetPtr readSize
    return (readSize, offsetPtr)
