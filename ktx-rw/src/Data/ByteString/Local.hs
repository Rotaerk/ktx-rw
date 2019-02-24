module Data.ByteString.Local (
  unsafeFromByteString
) where

import Data.ByteString
import qualified Data.ByteString.Internal as BSI
import Foreign.ForeignPtr
import Foreign.Storable
import System.IO.Unsafe

unsafeFromByteString :: Storable a => ByteString -> a
unsafeFromByteString bs =
  unsafeDupablePerformIO $ do
    let (fp, offset, _) = BSI.toForeignPtr bs
    withForeignPtr fp $ \p -> peekByteOff p offset
