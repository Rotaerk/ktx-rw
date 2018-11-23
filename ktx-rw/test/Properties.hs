{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Properties where

import Codec.Image.Ktx
import Control.Monad
import Data.Bits
import Data.List
import Data.Word
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.Directory
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen.Unsafe
import Test.QuickCheck.Monadic

prop_alignedValueVsBoundary :: Word32 -> Property
prop_alignedValueVsBoundary n = forAll boundaries $ \b -> alignTo b n `rem` b == 0

prop_alignedValueVsValue :: Word32 -> Property
prop_alignedValueVsValue n = forAll boundaries $ \b -> let d = alignTo b n - n in 0 <= d && d < b

boundaries :: forall n. FiniteBits n => Gen n
boundaries = bit <$> choose (0, finiteBitSize (undefined :: n) - 1)

prop_byteSwappedWord8BufferUnchanged :: [Word8] -> Property
prop_byteSwappedWord8BufferUnchanged words = monadicIO $ do
  byteSwappedWords <- run $ transformInBuffer words (byteSwapWordsInPlace 1)
  assert $ byteSwappedWords == words

prop_byteSwappedWord16BufferVsList :: [Word16] -> Property
prop_byteSwappedWord16BufferVsList words = monadicIO $ do
  byteSwappedWords <- run $ transformInBuffer words (byteSwapWordsInPlace 2)
  assert $ byteSwappedWords == fmap byteSwap16 words

prop_byteSwappedWord32BufferVsList :: [Word32] -> Property
prop_byteSwappedWord32BufferVsList words = monadicIO $ do
  byteSwappedWords <- run $ transformInBuffer words (byteSwapWordsInPlace 4)
  assert $ byteSwappedWords == fmap byteSwap32 words

prop_mapInPlaceMatchesMap :: [Word32] -> Property
prop_mapInPlaceMatchesMap words = monadicIO $ do
  mappedWords <- run $ transformInBuffer words (mapInPlace transform)
  assert $ mappedWords == fmap transform words
  where
    transform = (+1)

transformInBuffer :: Storable a => [a] -> (Ptr a -> Int -> IO ()) -> IO [a]
transformInBuffer as transform = do
  withArray as $ \ptr -> do
    transform ptr len
    peekArray len ptr
  where
    len = length as

-- Warning: QuickCheck magic ahead
return []
runTests :: IO Bool
runTests = $quickCheckAll
