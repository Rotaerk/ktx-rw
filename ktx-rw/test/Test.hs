{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Test where

import Control.Monad.Catch
import Control.Monad.Reader
import qualified Data.ByteString as BS
import Data.Functor
import Data.List
import Distribution.TestSuite
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import System.Directory hiding (getFileSize)
import System.FilePath.Posix
import System.IO

import Codec.Image.Ktx.Read
import Control.Monad.FileReader
import Control.Monad.BufferWriter

import qualified Properties

tests :: IO [Test]
tests = do
  textureFileNames <- getTextureFileNames
  return $ propertiesTest : sequence ktxTests textureFileNames

propertiesTest :: Test
propertiesTest =
  simpleTest "QuickCheck properties" $
    Properties.runTests <&> \case
      True -> Finished Pass
      False -> Finished (Fail "Some properties weren't satisfied.")

ktxTests :: [[FilePath] -> Test]
ktxTests =
  [
    ktxTest "Check Identifier" (\filePath ->
      withBinaryFile filePath ReadMode . runFileReaderT $
      try readAndCheckIdentifier >>= \case
        Left (SomeException ex) -> return . Fail $ displayException ex
        Right _ -> do
          offset <- fromIntegral <$> getByteOffsetInFile
          return $
            if offset /= identifierSize then
              Fail $ "At wrong offset (" ++ show offset ++ ") after reading identifier."
            else
              Pass
    ),

    ktxTest "Read Header" (\filePath ->
      withBinaryFile filePath ReadMode . runFileReaderT $
      readAndCheckIdentifier >> try readHeader >>= \case
        Left (SomeException ex) -> return . Fail $ displayException ex
        Right _ -> do
          offset <- fromIntegral <$> getByteOffsetInFile
          return $
            if offset /= identifierSize + headerSize then
              Fail $ "At wrong offset (" ++ show offset ++ ") after reading header."
            else
              Pass
    ),

    ktxTest "Skip Metadata" (\filePath ->
      withBinaryFile filePath ReadMode . runFileReaderT $
      readAndCheckIdentifier >> readHeader >>= runKtxBodyReaderT (
        try skipMetadata >>= \case
          Left (SomeException ex) -> return . Fail $ displayException ex
          Right _ -> do
            offset <- fromIntegral <$> getByteOffsetInFile
            expectedMetadataSize <- fromIntegral . header'bytesOfKeyValueData <$> ask
            return $
              if offset /= identifierSize + headerSize + expectedMetadataSize then
                Fail $ "At wrong offset (" ++ show offset ++ ") after skipping metadata."
              else
                Pass
      )
    ),

    ktxTest "Read Metadata" (\filePath ->
      withBinaryFile filePath ReadMode . runFileReaderT $
      readAndCheckIdentifier >> readHeader >>= runKtxBodyReaderT (
        try readMetadata >>= \case
          Left (SomeException ex) -> return . Fail $ displayException ex
          Right metadata -> do
            let metadataSize = getMetadataSize metadata
            offset <- fromIntegral <$> getByteOffsetInFile
            expectedMetadataSize <- fromIntegral . header'bytesOfKeyValueData <$> ask
            return $
              if offset /= identifierSize + headerSize + expectedMetadataSize then
                Fail $ "At wrong offset (" ++ show offset ++ ") after reading metadata."
              else if metadataSize /= expectedMetadataSize then
                Fail $ "Adjusted size of captured metadata (" ++ show metadataSize ++ ") inconsistent with expected size (" ++ show expectedMetadataSize ++ ")."
              else
                Pass
      )
    ),

    ktxTest "Get Texture Size" (\filePath ->
      withBinaryFile filePath ReadMode . runFileReaderT $
      readAndCheckIdentifier >> readHeader >>= runKtxBodyReaderT (do
        skipMetadata
        textureDataSize <- getTextureDataSize
        fileSize <- fromIntegral <$> getFileSize
        offset <- fromIntegral <$> getByteOffsetInFile
        imageSizeFieldsSize <- getImageSizeFieldsSize <$> ask
        let expectedTextureDataSize = fileSize - offset - imageSizeFieldsSize
        return $
          if textureDataSize /= expectedTextureDataSize then
            Fail $ "Texture data size (" ++ show textureDataSize ++ ") inconsistent with expected size (" ++ show expectedTextureDataSize ++ ")."
          else
            Pass
      )
    ),

    ktxTest "Read Texture Data Into Buffer" (\filePath -> do
      withBinaryFile filePath ReadMode . runFileReaderT $
        readAndCheckIdentifier >> readHeader >>= runKtxBodyReaderT (do
          skipMetadata
          bufferSize <- getTextureDataSize
          header <- ask
          bracket (liftIO $ mallocArray bufferSize) (liftIO . free) $ \bufferPtr ->
            try (runBufferWriterT readTextureDataIntoBuffer (bufferPtr, bufferSize) 0) >>= \case
              Left (SomeException ex) -> return . Fail $ displayException ex
              Right (bufferRegions, finalOffset) ->
                if finalOffset /= bufferSize then
                  return . Fail $ "The buffer of size " ++ show bufferSize ++ " was not completely filled.  Writing ended at offset " ++ show finalOffset ++ "."
                else
                  case bufferRegions of
                    SimpleBufferRegions _ ->
                      if isNonArrayCubeMap header then
                        return . Fail $ "Expected non-array cube-map buffer regions, but got simple buffer regions."
                      else
                        return Pass
                    NonArrayCubeMapBufferRegions _ ->
                      if not (isNonArrayCubeMap header) then
                        return . Fail $ "Expected simple buffer regions, but got non-array cube-map buffer regions."
                      else
                        return Pass
        )
    ),

    ktxTest "Read KTX File" (\filePath -> do
      try (readKtxFile filePath mallocArray free) >>= \case
        Left (SomeException ex) -> return . Fail $ displayException ex
        Right _ -> return Pass
    )
  ]

identifierSize :: Int
identifierSize = 12

headerSize :: Int
headerSize = 4 * 13

-- The size of the metadata block in the file based on the size of the captured Metadata
getMetadataSize :: Metadata -> Int
getMetadataSize = sum . fmap keyValuePairSize
  where
  keyValuePairSize (k, v) =
    let
      keyAndValueByteSize = BS.length k + 1 + BS.length v
      valuePadding = 3 - ((keyAndValueByteSize + 3) `mod` 4)
    in
      4 + keyAndValueByteSize + valuePadding

getImageSizeFieldsSize :: Header -> Int
getImageSizeFieldsSize = (4 *) . fromIntegral . header'numberOfMipmapLevels

ktxTest :: String -> (FilePath -> IO Result) -> [FilePath] -> Test
ktxTest testName fileTest textureFileNames =
  testGroup testName $ textureFileNames <&> \fileName ->
    simpleTest (testName ++ ": " ++ fileName) $ Finished <$> fileTest (texturesDir </> fileName)

texturesDir :: FilePath
texturesDir = "textures"

getTextureFileNames :: IO [FilePath]
getTextureFileNames =
  doesPathExist texturesDir >>= \case
    True -> filter (".ktx" `isSuffixOf`) <$> listDirectory texturesDir
    False -> return []

simpleTest :: String -> IO Progress -> Test
simpleTest n r =
  Test $ TestInstance {
    run = r,
    name = n,
    tags = [],
    options = [],
    setOption = \_ _ -> Left "No options accepted."
  }
