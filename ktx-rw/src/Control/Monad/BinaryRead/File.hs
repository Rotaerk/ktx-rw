{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.BinaryRead.File (
  BinaryFileRead (),
  runBinaryFileReadOn,
  runBinaryFileRead
) where
    
import Control.Monad.BinaryRead.Class
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import System.IO

newtype BinaryFileRead a =
  BinaryFileRead { unBinaryFileRead :: ReaderT Handle IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

runBinaryFileReadOn :: FilePath -> BinaryFileRead a -> IO a
runBinaryFileReadOn filePath bfr = withBinaryFile filePath ReadMode $ runReaderT . unBinaryFileRead $ bfr

runBinaryFileRead :: BinaryFileRead a -> FilePath -> IO a
runBinaryFileRead = flip runBinaryFileReadOn

liftWithHandle :: (Handle -> IO a) -> BinaryFileRead a
liftWithHandle f = BinaryFileRead ask >>= liftIO . f

instance MonadBinaryRead BinaryFileRead where
  type BinaryReadBookmark BinaryFileRead = HandlePosn
  dataSize = liftWithHandle hFileSize
  createBookmark = liftWithHandle hGetPosn
  goToBookmark = liftIO . hSetPosn
  seekRel o = liftWithHandle $ \h -> hSeek h RelativeSeek o
  tryReadToPtr p c = liftWithHandle $ \h -> hGetBuf h p c
  tryReadBS c = liftWithHandle $ \h -> BS.hGet h c
