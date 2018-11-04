{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Codec.Image.Ktx where
  
import Codec.Image.Ktx.Types
import Codec.Image.Ktx.Parser

import Control.Exception
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Functor
import System.IO

identifierSize :: Int
identifierSize = 12

endiannessSize :: Int
endiannessSize = 4

headerSize :: Int
headerSize = 12 * 4

data ReadException = ReadException String deriving (Eq, Show, Read)

instance Exception ReadException where
  displayException (ReadException message) = "Invalid KTX file: " ++ message

throwReadException :: String -> a
throwReadException = throw . ReadException

parseStrictly :: Parser a -> ByteString -> a
parseStrictly p i =
  case parse p i of
    Done _ result -> result
    Fail rest contexts message -> throwReadException $ "Fail (Rest = '" ++ show rest ++ "', Contexts = " ++ show contexts ++ ", Message = '" ++ message ++ "')"
    Partial _ -> throwReadException $ "Partial parse."

getAndParseStrictly :: IO ByteString -> Parser a -> IO a
getAndParseStrictly get p = parseStrictly p <$> get

readHeader :: FilePath -> IO Header
readHeader filePath =
  withFile filePath ReadMode $ \f ->
    getAndParseStrictly (BS.hGetSome f $ identifierSize + endiannessSize + headerSize) $ do
      fileIdentifier
      re <- relativeEndianness
      withRelativeEndianness re header

readMetadata :: FilePath -> IO [(ByteString, ByteString)]
readMetadata filePath =
  withFile filePath ReadMode $ \f -> do
    (re, header) <-
      getAndParseStrictly (BS.hGetSome f $ identifierSize + endiannessSize + headerSize) $ do
        fileIdentifier
        re <- relativeEndianness
        (re,) <$> withRelativeEndianness re header
    let bytesOfKeyValueData = fromIntegral $ header'bytesOfKeyValueData header
    getAndParseStrictly (BS.hGetSome f bytesOfKeyValueData) $
      withRelativeEndianness re (keyValueData bytesOfKeyValueData)
