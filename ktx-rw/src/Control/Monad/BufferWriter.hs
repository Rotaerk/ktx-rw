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
  
type BufferWriterEnv = (Ptr Word8, Size)

newtype BufferWriterT m a = BufferWriterT { unBufferWriterT :: ReaderT BufferWriterEnv (StateT Offset m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader BufferWriterEnv, MonadState Offset)

instance MonadTrans BufferWriterT where
  lift = BufferWriterT . lift . lift

newtype BufferWriteException = BufferWriteException String deriving (Eq, Show, Read)

instance Exception BufferWriteException where
  displayException (BufferWriteException message) = message

throwBufferWriteException :: MonadThrow m => String -> m a
throwBufferWriteException = throwM . BufferWriteException

buildBufferWriterT :: Monad m => (BufferWriterEnv -> Offset -> m (a, Offset)) -> BufferWriterT m a
buildBufferWriterT f = BufferWriterT . ReaderT $ StateT . f

runBufferWriterT :: Monad m => BufferWriterT m a -> BufferWriterEnv -> Offset -> m (a, Offset)
runBufferWriterT bw = runStateT . (runReaderT . unBufferWriterT) bw

runBufferWriterTOn :: Monad m => BufferWriterEnv -> Offset -> BufferWriterT m a -> m (a, Offset)
runBufferWriterTOn env initialOffset bw = runBufferWriterT bw env initialOffset

evalBufferWriterT :: Monad m => BufferWriterT m a -> BufferWriterEnv -> Offset -> m a
evalBufferWriterT bw env initialOffset = fst <$> runBufferWriterT bw env initialOffset

evalBufferWriterTOn :: Monad m => BufferWriterEnv -> Offset -> BufferWriterT m a -> m a
evalBufferWriterTOn env initialOffset bw = fst <$> runBufferWriterTOn env initialOffset bw

writeToBufferWith :: Monad m => (Ptr Word8 -> Size -> m (a, Size)) -> BufferWriterT m (a, Offset)
writeToBufferWith write =
  buildBufferWriterT $ \(bufferPtr, bufferSize) offset -> do
    (a, numBytesWritten) <- write (bufferPtr `plusPtr` offset) (bufferSize - offset)
    return ((a, offset), offset + numBytesWritten)
