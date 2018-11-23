{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Test where

import Data.Functor
import Data.List
import Distribution.TestSuite
import Foreign.Marshal.Array
import System.Directory
import System.FilePath.Posix
import System.IO

import Codec.Image.Ktx

import qualified Properties

tests :: IO [Test]
tests = do
  textureFileNames <- getTextureFileNames
  return
    [
      simpleTest "QuickCheck properties" $
        Properties.runTests <&> \case
          True -> Finished Pass
          False -> Finished (Fail "Some properties weren't satisfied.")
      ,
      -- Need to think of how to test for overconsuming the file.
      -- If necessary, I can make a subgroup for each file, and then within those have multiple tests for that file.
      testGroup "Read KTX files" $ textureFileNames <&> \fileName ->
        simpleTest ("Read '" ++ fileName ++ "'") $
          withBinaryFile (texturesDir </> fileName) ReadMode $ \fileHandle ->
            readKtxHandle fileHandle skipMetadata $ \header () (fromIntegral -> size) fillBuffer -> do
              void $ allocaArray size $ fillBuffer
              hIsEOF fileHandle <&> \case
                True -> Finished Pass
                False -> Finished (Fail "Did not fully consume the file.")
    ]

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
