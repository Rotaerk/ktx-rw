module Codec.Image.Ktx.Types where

import Data.Word
import qualified Data.Set as Set
import Data.ByteString (ByteString)

import Codec.Image.Ktx.GlConstants

data Header =
  Header {
    header'relativeEndianness :: RelativeEndianness,
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

isCubeMap :: Header -> Bool
isCubeMap = (6 ==) . header'numberOfFaces

isArray :: Header -> Bool
isArray = (0 <) . header'numberOfArrayElements

isNonArrayCubeMap :: Header -> Bool
isNonArrayCubeMap h = isCubeMap h && not (isArray h)

isCompressed :: Header -> Bool
isCompressed = (0 ==) . header'glType

hasPalettedInternalFormat :: Header -> Bool
hasPalettedInternalFormat = (`Set.member` palettedFormats) . header'glInternalFormat
  where
    palettedFormats =
      Set.fromList [
        GL_PALETTE4_RGB8_OES,
        GL_PALETTE4_RGBA8_OES,
        GL_PALETTE4_R5_G6_B5_OES,
        GL_PALETTE4_RGBA4_OES,
        GL_PALETTE4_RGB5_A1_OES,
        GL_PALETTE8_RGB8_OES,
        GL_PALETTE8_RGBA8_OES,
        GL_PALETTE8_R5_G6_B5_OES,
        GL_PALETTE8_RGBA4_OES,
        GL_PALETTE8_RGB5_A1_OES
      ]

effectiveNumberOfMipmapLevels :: Header -> Word32
effectiveNumberOfMipmapLevels h = if hasPalettedInternalFormat h then 1 else replace 0 1 $ header'numberOfMipmapLevels h

replace :: Eq a => a -> a -> a -> a
replace match replacement value | value == match = replacement
replace _ _ value = value

data RelativeEndianness = SameEndian | FlipEndian deriving (Show, Eq)

type Metadata = [(ByteString, ByteString)]
