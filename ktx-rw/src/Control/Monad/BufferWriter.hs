{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Control.Monad.BufferWriter where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Word
import Foreign.Ptr

type Offset = Int
type Size = Int
type Buffer = (Ptr Word8, Size)

newtype BufferWriterT m a = BufferWriterT { unBufferWriterT :: ReaderT Buffer (StateT Offset m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Buffer, MonadState Offset)

instance MonadTrans BufferWriterT where
  lift = BufferWriterT . lift . lift

newtype BufferWriteException = BufferWriteException String deriving (Eq, Show, Read)

instance Exception BufferWriteException where
  displayException (BufferWriteException message) = message

throwBufferWriteException :: MonadThrow m => String -> m a
throwBufferWriteException = throwM . BufferWriteException

buildBufferWriterT :: Monad m => (Buffer -> Offset -> m (a, Offset)) -> BufferWriterT m a
buildBufferWriterT f = BufferWriterT . ReaderT $ StateT . f

runBufferWriterT :: Monad m => BufferWriterT m a -> Buffer -> Offset -> m (a, Offset)
runBufferWriterT bw = runStateT . (runReaderT . unBufferWriterT) bw

runBufferWriterTOn :: Monad m => Buffer -> Offset -> BufferWriterT m a -> m (a, Offset)
runBufferWriterTOn buffer initialOffset bw = runBufferWriterT bw buffer initialOffset

evalBufferWriterT :: Monad m => BufferWriterT m a -> Buffer -> Offset -> m a
evalBufferWriterT bw buffer initialOffset = fst <$> runBufferWriterT bw buffer initialOffset

evalBufferWriterTOn :: Monad m => Buffer -> Offset -> BufferWriterT m a -> m a
evalBufferWriterTOn buffer initialOffset bw = fst <$> runBufferWriterTOn buffer initialOffset bw

writeToBufferWith :: Monad m => (Ptr Word8 -> Size -> m (a, Size)) -> BufferWriterT m (a, Offset)
writeToBufferWith write =
  buildBufferWriterT $ \(bufferPtr, bufferSize) offset -> do
    (a, numBytesWritten) <- write (bufferPtr `plusPtr` offset) (bufferSize - offset)
    return ((a, offset), offset + numBytesWritten)
