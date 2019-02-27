module Control.Monad.BinaryRead.Handle where

import Control.Monad.BinaryRead.Class

data BinaryFileRead a = BinaryFileRead { runBinaryReadHandle :: ReaderT Handle IO a
