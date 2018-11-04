module Codec.Image.Ktx.Types where

import Data.Word

data Header =
  Header {
    header'glType :: Word32,
    header'glTypeSize :: Word32,
    header'glFormat :: Word32,
    header'glInternalFormat :: Word32,
    header'glBaseInternalFormat :: Word32,
    header'pixelWidth :: Word32,
    header'pixelHeight :: Word32,
    header'pixelDepth :: Word32,
    header'numberOfArrayElements :: Word32,
    header'numberOfFaces :: Word32,
    header'numberOfMipmapLevels :: Word32,
    header'bytesOfKeyValueData :: Word32
  }
  deriving (Show)

data RelativeEndianness = SameEndian | FlipEndian deriving (Show, Eq)
