{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

-- This module is a port of the gl_format.h file included under /ktx-rw/refsrc.

module Codec.Image.Ktx.GLConstants where

import Control.Exception
import Data.Bits
import Data.Word

type GlEnum = Word32

newtype GlConstantException = GlConstantException String deriving (Eq, Show, Read)

instance Exception GlConstantException where
  displayException (GlConstantException message) = message

throwGlConstantException :: String -> a
throwGlConstantException = throw . GlConstantException

{-
================================================================================================================================

Format to glTexImage2D and glTexImage3D.

================================================================================================================================
-}

pattern GL_RED = 0x1903 :: GlEnum -- same as GL_RED_EXT
pattern GL_GREEN = 0x1904 :: GlEnum -- deprecated
pattern GL_BLUE = 0x1905 :: GlEnum -- deprecated
pattern GL_ALPHA = 0x1906 :: GlEnum -- deprecated
pattern GL_LUMINANCE = 0x1909 :: GlEnum -- deprecated
pattern GL_SLUMINANCE = 0x8C46 :: GlEnum -- deprecated, same as GL_SLUMINANCE_EXT
pattern GL_LUMINANCE_ALPHA = 0x190A :: GlEnum -- deprecated
pattern GL_SLUMINANCE_ALPHA = 0x8C44 :: GlEnum -- deprecated, same as GL_SLUMINANCE_ALPHA_EXT
pattern GL_INTENSITY = 0x8049 :: GlEnum -- deprecated, same as GL_INTENSITY_EXT
pattern GL_RG = 0x8227 :: GlEnum -- same as GL_RG_EXT
pattern GL_RGB = 0x1907 :: GlEnum
pattern GL_BGR = 0x80E0 :: GlEnum -- same as GL_BGR_EXT
pattern GL_RGBA = 0x1908 :: GlEnum
pattern GL_BGRA = 0x80E1 :: GlEnum -- same as GL_BGRA_EXT
pattern GL_RED_INTEGER = 0x8D94 :: GlEnum -- same as GL_RED_INTEGER_EXT
pattern GL_GREEN_INTEGER = 0x8D95 :: GlEnum -- deprecated, same as GL_GREEN_INTEGER_EXT
pattern GL_BLUE_INTEGER = 0x8D96 :: GlEnum -- deprecated, same as GL_BLUE_INTEGER_EXT
pattern GL_ALPHA_INTEGER = 0x8D97 :: GlEnum -- deprecated, same as GL_ALPHA_INTEGER_EXT
pattern GL_LUMINANCE_INTEGER = 0x8D9C :: GlEnum -- deprecated, same as GL_LUMINANCE_INTEGER_EXT
pattern GL_LUMINANCE_ALPHA_INTEGER = 0x8D9D :: GlEnum -- deprecated, same as GL_LUMINANCE_ALPHA_INTEGER_EXT
pattern GL_RG_INTEGER = 0x8228 :: GlEnum -- same as GL_RG_INTEGER_EXT
pattern GL_RGB_INTEGER = 0x8D98 :: GlEnum -- same as GL_RGB_INTEGER_EXT
pattern GL_BGR_INTEGER = 0x8D9A :: GlEnum -- same as GL_BGR_INTEGER_EXT
pattern GL_RGBA_INTEGER = 0x8D99 :: GlEnum -- same as GL_RGBA_INTEGER_EXT
pattern GL_BGRA_INTEGER = 0x8D9B :: GlEnum -- same as GL_BGRA_INTEGER_EXT
pattern GL_COLOR_INDEX = 0x1900 :: GlEnum -- deprecated
pattern GL_STENCIL_INDEX = 0x1901 :: GlEnum
pattern GL_DEPTH_COMPONENT = 0x1902 :: GlEnum
pattern GL_DEPTH_STENCIL = 0x84F9 :: GlEnum -- same as GL_DEPTH_STENCIL_NV and GL_DEPTH_STENCIL_EXT and GL_DEPTH_STENCIL_OES

{-
================================================================================================================================

Type to glTexImage2D, glTexImage3D and glVertexAttribPointer.

================================================================================================================================
-}

pattern GL_BYTE = 0x1400 :: GlEnum
pattern GL_UNSIGNED_BYTE = 0x1401 :: GlEnum
pattern GL_SHORT = 0x1402 :: GlEnum
pattern GL_UNSIGNED_SHORT = 0x1403 :: GlEnum
pattern GL_INT = 0x1404 :: GlEnum
pattern GL_UNSIGNED_INT = 0x1405 :: GlEnum
pattern GL_INT64 = 0x140E :: GlEnum -- same as GL_INT64_NV and GL_INT64_ARB
pattern GL_UNSIGNED_INT64 = 0x140F :: GlEnum -- same as GL_UNSIGNED_INT64_NV and GL_UNSIGNED_INT64_ARB
pattern GL_HALF_FLOAT = 0x140B :: GlEnum -- same as GL_HALF_FLOAT_NV and GL_HALF_FLOAT_ARB
pattern GL_HALF_FLOAT_OES = 0x8D61 :: GlEnum -- Note that this different from GL_HALF_FLOAT.
pattern GL_FLOAT = 0x1406 :: GlEnum
pattern GL_DOUBLE = 0x140A :: GlEnum -- same as GL_DOUBLE_EXT
pattern GL_UNSIGNED_BYTE_3_3_2 = 0x8032 :: GlEnum -- same as GL_UNSIGNED_BYTE_3_3_2_EXT
pattern GL_UNSIGNED_BYTE_2_3_3_REV = 0x8362 :: GlEnum -- same as GL_UNSIGNED_BYTE_2_3_3_REV_EXT
pattern GL_UNSIGNED_SHORT_5_6_5 = 0x8363 :: GlEnum -- same as GL_UNSIGNED_SHORT_5_6_5_EXT
pattern GL_UNSIGNED_SHORT_5_6_5_REV = 0x8364 :: GlEnum -- same as GL_UNSIGNED_SHORT_5_6_5_REV_EXT
pattern GL_UNSIGNED_SHORT_4_4_4_4 = 0x8033 :: GlEnum -- same as GL_UNSIGNED_SHORT_4_4_4_4_EXT
pattern GL_UNSIGNED_SHORT_4_4_4_4_REV = 0x8365 :: GlEnum -- same as GL_UNSIGNED_SHORT_4_4_4_4_REV_IMG and GL_UNSIGNED_SHORT_4_4_4_4_REV_EXT
pattern GL_UNSIGNED_SHORT_5_5_5_1 = 0x8034 :: GlEnum -- same as GL_UNSIGNED_SHORT_5_5_5_1_EXT
pattern GL_UNSIGNED_SHORT_1_5_5_5_REV = 0x8366 :: GlEnum -- same as GL_UNSIGNED_SHORT_1_5_5_5_REV_EXT
pattern GL_UNSIGNED_INT_8_8_8_8 = 0x8035 :: GlEnum -- same as GL_UNSIGNED_INT_8_8_8_8_EXT
pattern GL_UNSIGNED_INT_8_8_8_8_REV = 0x8367 :: GlEnum -- same as GL_UNSIGNED_INT_8_8_8_8_REV_EXT
pattern GL_UNSIGNED_INT_10_10_10_2 = 0x8036 :: GlEnum -- same as GL_UNSIGNED_INT_10_10_10_2_EXT
pattern GL_UNSIGNED_INT_2_10_10_10_REV = 0x8368 :: GlEnum -- same as GL_UNSIGNED_INT_2_10_10_10_REV_EXT
pattern GL_UNSIGNED_INT_10F_11F_11F_REV = 0x8C3B :: GlEnum -- same as GL_UNSIGNED_INT_10F_11F_11F_REV_EXT
pattern GL_UNSIGNED_INT_5_9_9_9_REV = 0x8C3E :: GlEnum -- same as GL_UNSIGNED_INT_5_9_9_9_REV_EXT
pattern GL_UNSIGNED_INT_24_8 = 0x84FA :: GlEnum -- same as GL_UNSIGNED_INT_24_8_NV and GL_UNSIGNED_INT_24_8_EXT and GL_UNSIGNED_INT_24_8_OES
pattern GL_FLOAT_32_UNSIGNED_INT_24_8_REV = 0x8DAD :: GlEnum -- same as GL_FLOAT_32_UNSIGNED_INT_24_8_REV_NV and GL_FLOAT_32_UNSIGNED_INT_24_8_REV_ARB

{-
================================================================================================================================

Internal format to glTexImage2D, glTexImage3D, glCompressedTexImage2D, glCompressedTexImage3D, glTexStorage2D, glTexStorage3D

================================================================================================================================
-}

--
-- 8 bits per component
--

pattern GL_R8 = 0x8229 :: GlEnum -- same as GL_R8_EXT
pattern GL_RG8 = 0x822B :: GlEnum -- same as GL_RG8_EXT
pattern GL_RGB8 = 0x8051 :: GlEnum -- same as GL_RGB8_EXT and GL_RGB8_OES
pattern GL_RGBA8 = 0x8058 :: GlEnum -- same as GL_RGBA8_EXT and GL_RGBA8_OES

pattern GL_R8_SNORM = 0x8F94 :: GlEnum
pattern GL_RG8_SNORM = 0x8F95 :: GlEnum
pattern GL_RGB8_SNORM = 0x8F96 :: GlEnum
pattern GL_RGBA8_SNORM = 0x8F97 :: GlEnum

pattern GL_R8UI = 0x8232 :: GlEnum
pattern GL_RG8UI = 0x8238 :: GlEnum
pattern GL_RGB8UI = 0x8D7D :: GlEnum -- same as GL_RGB8UI_EXT
pattern GL_RGBA8UI = 0x8D7C :: GlEnum -- same as GL_RGBA8UI_EXT

pattern GL_R8I = 0x8231 :: GlEnum
pattern GL_RG8I = 0x8237 :: GlEnum
pattern GL_RGB8I = 0x8D8F :: GlEnum -- same as GL_RGB8I_EXT
pattern GL_RGBA8I = 0x8D8E :: GlEnum -- same as GL_RGBA8I_EXT

pattern GL_SR8 = 0x8FBD :: GlEnum -- same as GL_SR8_EXT
pattern GL_SRG8 = 0x8FBE :: GlEnum -- same as GL_SRG8_EXT
pattern GL_SRGB8 = 0x8C41 :: GlEnum -- same as GL_SRGB8_EXT
pattern GL_SRGB8_ALPHA8 = 0x8C43 :: GlEnum -- same as GL_SRGB8_ALPHA8_EXT

--
-- 16 bits per component
--

pattern GL_R16 = 0x822A :: GlEnum -- same as GL_R16_EXT
pattern GL_RG16 = 0x822C :: GlEnum -- same as GL_RG16_EXT
pattern GL_RGB16 = 0x8054 :: GlEnum -- same as GL_RGB16_EXT
pattern GL_RGBA16 = 0x805B :: GlEnum -- same as GL_RGBA16_EXT

pattern GL_R16_SNORM = 0x8F98 :: GlEnum -- same as GL_R16_SNORM_EXT
pattern GL_RG16_SNORM = 0x8F99 :: GlEnum -- same as GL_RG16_SNORM_EXT
pattern GL_RGB16_SNORM = 0x8F9A :: GlEnum -- same as GL_RGB16_SNORM_EXT
pattern GL_RGBA16_SNORM = 0x8F9B :: GlEnum -- same as GL_RGBA16_SNORM_EXT

pattern GL_R16UI = 0x8234 :: GlEnum
pattern GL_RG16UI = 0x823A :: GlEnum
pattern GL_RGB16UI = 0x8D77 :: GlEnum -- same as GL_RGB16UI_EXT
pattern GL_RGBA16UI = 0x8D76 :: GlEnum -- same as GL_RGBA16UI_EXT

pattern GL_R16I = 0x8233 :: GlEnum
pattern GL_RG16I = 0x8239 :: GlEnum
pattern GL_RGB16I = 0x8D89 :: GlEnum -- same as GL_RGB16I_EXT
pattern GL_RGBA16I = 0x8D88 :: GlEnum -- same as GL_RGBA16I_EXT

pattern GL_R16F = 0x822D :: GlEnum -- same as GL_R16F_EXT
pattern GL_RG16F = 0x822F :: GlEnum -- same as GL_RG16F_EXT
pattern GL_RGB16F = 0x881B :: GlEnum -- same as GL_RGB16F_EXT and GL_RGB16F_ARB
pattern GL_RGBA16F = 0x881A :: GlEnum -- sama as GL_RGBA16F_EXT and GL_RGBA16F_ARB

--
-- 32 bits per component
--

pattern GL_R32UI = 0x8236 :: GlEnum
pattern GL_RG32UI = 0x823C :: GlEnum
pattern GL_RGB32UI = 0x8D71 :: GlEnum -- same as GL_RGB32UI_EXT
pattern GL_RGBA32UI = 0x8D70 :: GlEnum -- same as GL_RGBA32UI_EXT

pattern GL_R32I = 0x8235 :: GlEnum
pattern GL_RG32I = 0x823B :: GlEnum
pattern GL_RGB32I = 0x8D83 :: GlEnum -- same as GL_RGB32I_EXT 
pattern GL_RGBA32I = 0x8D82 :: GlEnum -- same as GL_RGBA32I_EXT

pattern GL_R32F = 0x822E :: GlEnum -- same as GL_R32F_EXT
pattern GL_RG32F = 0x8230 :: GlEnum -- same as GL_RG32F_EXT
pattern GL_RGB32F = 0x8815 :: GlEnum -- same as GL_RGB32F_EXT and GL_RGB32F_ARB
pattern GL_RGBA32F = 0x8814 :: GlEnum -- same as GL_RGBA32F_EXT and GL_RGBA32F_ARB

--
-- Packed
--

pattern GL_R3_G3_B2 = 0x2A10 :: GlEnum
pattern GL_RGB4 = 0x804F :: GlEnum -- same as GL_RGB4_EXT
pattern GL_RGB5 = 0x8050 :: GlEnum -- same as GL_RGB5_EXT
pattern GL_RGB565 = 0x8D62 :: GlEnum -- same as GL_RGB565_EXT and GL_RGB565_OES
pattern GL_RGB10 = 0x8052 :: GlEnum -- same as GL_RGB10_EXT
pattern GL_RGB12 = 0x8053 :: GlEnum -- same as GL_RGB12_EXT
pattern GL_RGBA2 = 0x8055 :: GlEnum -- same as GL_RGBA2_EXT
pattern GL_RGBA4 = 0x8056 :: GlEnum -- same as GL_RGBA4_EXT and GL_RGBA4_OES
pattern GL_RGBA12 = 0x805A :: GlEnum -- same as GL_RGBA12_EXT
pattern GL_RGB5_A1 = 0x8057 :: GlEnum -- same as GL_RGB5_A1_EXT and GL_RGB5_A1_OES
pattern GL_RGB10_A2 = 0x8059 :: GlEnum -- same as GL_RGB10_A2_EXT
pattern GL_RGB10_A2UI = 0x906F :: GlEnum
pattern GL_R11F_G11F_B10F = 0x8C3A :: GlEnum -- same as GL_R11F_G11F_B10F_APPLE and GL_R11F_G11F_B10F_EXT
pattern GL_RGB9_E5 = 0x8C3D :: GlEnum -- same as GL_RGB9_E5_APPLE and GL_RGB9_E5_EXT

--
-- Alpha
--

pattern GL_ALPHA4 = 0x803B :: GlEnum -- deprecated, same as GL_ALPHA4_EXT
pattern GL_ALPHA8 = 0x803C :: GlEnum -- deprecated, same as GL_ALPHA8_EXT
pattern GL_ALPHA8_SNORM = 0x9014 :: GlEnum -- deprecated
pattern GL_ALPHA8UI_EXT = 0x8D7E :: GlEnum -- deprecated
pattern GL_ALPHA8I_EXT = 0x8D90 :: GlEnum -- deprecated
pattern GL_ALPHA12 = 0x803D :: GlEnum -- deprecated, same as GL_ALPHA12_EXT
pattern GL_ALPHA16 = 0x803E :: GlEnum -- deprecated, same as GL_ALPHA16_EXT
pattern GL_ALPHA16_SNORM = 0x9018 :: GlEnum -- deprecated
pattern GL_ALPHA16UI_EXT = 0x8D78 :: GlEnum -- deprecated
pattern GL_ALPHA16I_EXT = 0x8D8A :: GlEnum -- deprecated
pattern GL_ALPHA16F_ARB = 0x881C :: GlEnum -- deprecated, same as GL_ALPHA_FLOAT16_APPLE and GL_ALPHA_FLOAT16_ATI
pattern GL_ALPHA32UI_EXT = 0x8D72 :: GlEnum -- deprecated
pattern GL_ALPHA32I_EXT = 0x8D84 :: GlEnum -- deprecated
pattern GL_ALPHA32F_ARB = 0x8816 :: GlEnum -- deprecated, same as GL_ALPHA_FLOAT32_APPLE and GL_ALPHA_FLOAT32_ATI

--
-- Luminance
--

pattern GL_LUMINANCE4 = 0x803F :: GlEnum -- deprecated, same as GL_LUMINANCE4_EXT
pattern GL_LUMINANCE8 = 0x8040 :: GlEnum -- deprecated, same as GL_LUMINANCE8_EXT
pattern GL_LUMINANCE8_SNORM = 0x9015 :: GlEnum -- deprecated
pattern GL_SLUMINANCE8 = 0x8C47 :: GlEnum -- deprecated, same as GL_SLUMINANCE8_EXT
pattern GL_LUMINANCE8UI_EXT = 0x8D80 :: GlEnum -- deprecated
pattern GL_LUMINANCE8I_EXT = 0x8D92 :: GlEnum -- deprecated
pattern GL_LUMINANCE12 = 0x8041 :: GlEnum -- deprecated, same as GL_LUMINANCE12_EXT
pattern GL_LUMINANCE16 = 0x8042 :: GlEnum -- deprecated, same as GL_LUMINANCE16_EXT
pattern GL_LUMINANCE16_SNORM = 0x9019 :: GlEnum -- deprecated
pattern GL_LUMINANCE16UI_EXT = 0x8D7A :: GlEnum -- deprecated
pattern GL_LUMINANCE16I_EXT = 0x8D8C :: GlEnum -- deprecated
pattern GL_LUMINANCE16F_ARB = 0x881E :: GlEnum -- deprecated, same as GL_LUMINANCE_FLOAT16_APPLE and GL_LUMINANCE_FLOAT16_ATI
pattern GL_LUMINANCE32UI_EXT = 0x8D74 :: GlEnum -- deprecated
pattern GL_LUMINANCE32I_EXT = 0x8D86 :: GlEnum -- deprecated
pattern GL_LUMINANCE32F_ARB = 0x8818 :: GlEnum -- deprecated, same as GL_LUMINANCE_FLOAT32_APPLE and GL_LUMINANCE_FLOAT32_ATI

--
-- Luminance/Alpha
--

pattern GL_LUMINANCE4_ALPHA4 = 0x8043 :: GlEnum -- deprecated, same as GL_LUMINANCE4_ALPHA4_EXT
pattern GL_LUMINANCE6_ALPHA2 = 0x8044 :: GlEnum -- deprecated, same as GL_LUMINANCE6_ALPHA2_EXT
pattern GL_LUMINANCE8_ALPHA8 = 0x8045 :: GlEnum -- deprecated, same as GL_LUMINANCE8_ALPHA8_EXT
pattern GL_LUMINANCE8_ALPHA8_SNORM = 0x9016 :: GlEnum -- deprecated
pattern GL_SLUMINANCE8_ALPHA8 = 0x8C45 :: GlEnum -- deprecated, same as GL_SLUMINANCE8_ALPHA8_EXT
pattern GL_LUMINANCE_ALPHA8UI_EXT = 0x8D81 :: GlEnum -- deprecated
pattern GL_LUMINANCE_ALPHA8I_EXT = 0x8D93 :: GlEnum -- deprecated
pattern GL_LUMINANCE12_ALPHA4 = 0x8046 :: GlEnum -- deprecated, same as GL_LUMINANCE12_ALPHA4_EXT
pattern GL_LUMINANCE12_ALPHA12 = 0x8047 :: GlEnum -- deprecated, same as GL_LUMINANCE12_ALPHA12_EXT
pattern GL_LUMINANCE16_ALPHA16 = 0x8048 :: GlEnum -- deprecated, same as GL_LUMINANCE16_ALPHA16_EXT
pattern GL_LUMINANCE16_ALPHA16_SNORM = 0x901A :: GlEnum -- deprecated
pattern GL_LUMINANCE_ALPHA16UI_EXT = 0x8D7B :: GlEnum -- deprecated
pattern GL_LUMINANCE_ALPHA16I_EXT = 0x8D8D :: GlEnum -- deprecated
pattern GL_LUMINANCE_ALPHA16F_ARB = 0x881F :: GlEnum -- deprecated, same as GL_LUMINANCE_ALPHA_FLOAT16_APPLE and GL_LUMINANCE_ALPHA_FLOAT16_ATI
pattern GL_LUMINANCE_ALPHA32UI_EXT = 0x8D75 :: GlEnum -- deprecated
pattern GL_LUMINANCE_ALPHA32I_EXT = 0x8D87 :: GlEnum -- deprecated
pattern GL_LUMINANCE_ALPHA32F_ARB = 0x8819 :: GlEnum -- deprecated, same as GL_LUMINANCE_ALPHA_FLOAT32_APPLE and GL_LUMINANCE_ALPHA_FLOAT32_ATI

--
-- Intensity
--

pattern GL_INTENSITY4 = 0x804A :: GlEnum -- deprecated, same as GL_INTENSITY4_EXT
pattern GL_INTENSITY8 = 0x804B :: GlEnum -- deprecated, same as GL_INTENSITY8_EXT
pattern GL_INTENSITY8_SNORM = 0x9017 :: GlEnum -- deprecated
pattern GL_INTENSITY8UI_EXT = 0x8D7F :: GlEnum -- deprecated
pattern GL_INTENSITY8I_EXT = 0x8D91 :: GlEnum -- deprecated
pattern GL_INTENSITY12 = 0x804C :: GlEnum -- deprecated, same as GL_INTENSITY12_EXT
pattern GL_INTENSITY16 = 0x804D :: GlEnum -- deprecated, same as GL_INTENSITY16_EXT
pattern GL_INTENSITY16_SNORM = 0x901B :: GlEnum -- deprecated
pattern GL_INTENSITY16UI_EXT = 0x8D79 :: GlEnum -- deprecated
pattern GL_INTENSITY16I_EXT = 0x8D8B :: GlEnum -- deprecated
pattern GL_INTENSITY16F_ARB = 0x881D :: GlEnum -- deprecated, same as GL_INTENSITY_FLOAT16_APPLE and GL_INTENSITY_FLOAT16_ATI
pattern GL_INTENSITY32UI_EXT = 0x8D73 :: GlEnum -- deprecated
pattern GL_INTENSITY32I_EXT = 0x8D85 :: GlEnum -- deprecated
pattern GL_INTENSITY32F_ARB = 0x8817 :: GlEnum -- deprecated, same as GL_INTENSITY_FLOAT32_APPLE and GL_INTENSITY_FLOAT32_ATI

--
-- Generic compression
--

pattern GL_COMPRESSED_RED = 0x8225 :: GlEnum
pattern GL_COMPRESSED_ALPHA = 0x84E9 :: GlEnum -- deprecated, same as GL_COMPRESSED_ALPHA_ARB
pattern GL_COMPRESSED_LUMINANCE = 0x84EA :: GlEnum -- deprecated, same as GL_COMPRESSED_LUMINANCE_ARB
pattern GL_COMPRESSED_SLUMINANCE = 0x8C4A :: GlEnum -- deprecated, same as GL_COMPRESSED_SLUMINANCE_EXT
pattern GL_COMPRESSED_LUMINANCE_ALPHA = 0x84EB :: GlEnum -- deprecated, same as GL_COMPRESSED_LUMINANCE_ALPHA_ARB
pattern GL_COMPRESSED_SLUMINANCE_ALPHA = 0x8C4B :: GlEnum -- deprecated, same as GL_COMPRESSED_SLUMINANCE_ALPHA_EXT
pattern GL_COMPRESSED_INTENSITY = 0x84EC :: GlEnum -- deprecated, same as GL_COMPRESSED_INTENSITY_ARB
pattern GL_COMPRESSED_RG = 0x8226 :: GlEnum
pattern GL_COMPRESSED_RGB = 0x84ED :: GlEnum -- same as GL_COMPRESSED_RGB_ARB
pattern GL_COMPRESSED_RGBA = 0x84EE :: GlEnum -- same as GL_COMPRESSED_RGBA_ARB
pattern GL_COMPRESSED_SRGB = 0x8C48 :: GlEnum -- same as GL_COMPRESSED_SRGB_EXT
pattern GL_COMPRESSED_SRGB_ALPHA = 0x8C49 :: GlEnum -- same as GL_COMPRESSED_SRGB_ALPHA_EXT

--
-- FXT1
--

pattern GL_COMPRESSED_RGB_FXT1_3DFX = 0x86B0 :: GlEnum -- deprecated
pattern GL_COMPRESSED_RGBA_FXT1_3DFX = 0x86B1 :: GlEnum -- deprecated

--
-- S3TC/DXT/BC
--

pattern GL_COMPRESSED_RGB_S3TC_DXT1_EXT = 0x83F0 :: GlEnum
pattern GL_COMPRESSED_RGBA_S3TC_DXT1_EXT = 0x83F1 :: GlEnum
pattern GL_COMPRESSED_RGBA_S3TC_DXT3_EXT = 0x83F2 :: GlEnum
pattern GL_COMPRESSED_RGBA_S3TC_DXT5_EXT = 0x83F3 :: GlEnum

pattern GL_COMPRESSED_SRGB_S3TC_DXT1_EXT = 0x8C4C :: GlEnum
pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT = 0x8C4D :: GlEnum
pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT = 0x8C4E :: GlEnum
pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT = 0x8C4F :: GlEnum

pattern GL_COMPRESSED_LUMINANCE_LATC1_EXT = 0x8C70 :: GlEnum
pattern GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT = 0x8C72 :: GlEnum
pattern GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT = 0x8C71 :: GlEnum
pattern GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT = 0x8C73 :: GlEnum

pattern GL_COMPRESSED_RED_RGTC1 = 0x8DBB :: GlEnum -- same as GL_COMPRESSED_RED_RGTC1_EXT
pattern GL_COMPRESSED_RG_RGTC2 = 0x8DBD :: GlEnum -- same as GL_COMPRESSED_RG_RGTC2_EXT
pattern GL_COMPRESSED_SIGNED_RED_RGTC1 = 0x8DBC :: GlEnum -- same as GL_COMPRESSED_SIGNED_RED_RGTC1_EXT
pattern GL_COMPRESSED_SIGNED_RG_RGTC2 = 0x8DBE :: GlEnum -- same as GL_COMPRESSED_SIGNED_RG_RGTC2_EXT

pattern GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT = 0x8E8E :: GlEnum -- same as GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT_ARB
pattern GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT = 0x8E8F :: GlEnum -- same as GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT_ARB
pattern GL_COMPRESSED_RGBA_BPTC_UNORM = 0x8E8C :: GlEnum -- same as GL_COMPRESSED_RGBA_BPTC_UNORM_ARB 
pattern GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM = 0x8E8D :: GlEnum -- same as GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM_ARB

--
-- ETC
--

pattern GL_ETC1_RGB8_OES = 0x8D64 :: GlEnum

pattern GL_COMPRESSED_RGB8_ETC2 = 0x9274 :: GlEnum
pattern GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2 = 0x9276 :: GlEnum
pattern GL_COMPRESSED_RGBA8_ETC2_EAC = 0x9278 :: GlEnum

pattern GL_COMPRESSED_SRGB8_ETC2 = 0x9275 :: GlEnum
pattern GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2 = 0x9277 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC = 0x9279 :: GlEnum

pattern GL_COMPRESSED_R11_EAC = 0x9270 :: GlEnum
pattern GL_COMPRESSED_RG11_EAC = 0x9272 :: GlEnum
pattern GL_COMPRESSED_SIGNED_R11_EAC = 0x9271 :: GlEnum
pattern GL_COMPRESSED_SIGNED_RG11_EAC = 0x9273 :: GlEnum

--
-- PVRTC
--

pattern GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG = 0x8C01 :: GlEnum
pattern GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG = 0x8C00 :: GlEnum
pattern GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG = 0x8C03 :: GlEnum
pattern GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG = 0x8C02 :: GlEnum
pattern GL_COMPRESSED_RGBA_PVRTC_2BPPV2_IMG = 0x9137 :: GlEnum
pattern GL_COMPRESSED_RGBA_PVRTC_4BPPV2_IMG = 0x9138 :: GlEnum
pattern GL_COMPRESSED_SRGB_PVRTC_2BPPV1_EXT = 0x8A54 :: GlEnum
pattern GL_COMPRESSED_SRGB_PVRTC_4BPPV1_EXT = 0x8A55 :: GlEnum
pattern GL_COMPRESSED_SRGB_ALPHA_PVRTC_2BPPV1_EXT = 0x8A56 :: GlEnum
pattern GL_COMPRESSED_SRGB_ALPHA_PVRTC_4BPPV1_EXT = 0x8A57 :: GlEnum
pattern GL_COMPRESSED_SRGB_ALPHA_PVRTC_2BPPV2_IMG = 0x93F0 :: GlEnum
pattern GL_COMPRESSED_SRGB_ALPHA_PVRTC_4BPPV2_IMG = 0x93F1 :: GlEnum

--
-- ASTC
--

pattern GL_COMPRESSED_RGBA_ASTC_4x4_KHR = 0x93B0 :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_5x4_KHR = 0x93B1 :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_5x5_KHR = 0x93B2 :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_6x5_KHR = 0x93B3 :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_6x6_KHR = 0x93B4 :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_8x5_KHR = 0x93B5 :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_8x6_KHR = 0x93B6 :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_8x8_KHR = 0x93B7 :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_10x5_KHR = 0x93B8 :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_10x6_KHR = 0x93B9 :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_10x8_KHR = 0x93BA :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_10x10_KHR = 0x93BB :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_12x10_KHR = 0x93BC :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_12x12_KHR = 0x93BD :: GlEnum

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR = 0x93D0 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR = 0x93D1 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR = 0x93D2 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR = 0x93D3 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR = 0x93D4 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR = 0x93D5 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR = 0x93D6 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR = 0x93D7 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR = 0x93D8 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR = 0x93D9 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR = 0x93DA :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR = 0x93DB :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR = 0x93DC :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR = 0x93DD :: GlEnum

pattern GL_COMPRESSED_RGBA_ASTC_3x3x3_OES = 0x93C0 :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_4x3x3_OES = 0x93C1 :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_4x4x3_OES = 0x93C2 :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_4x4x4_OES = 0x93C3 :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_5x4x4_OES = 0x93C4 :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_5x5x4_OES = 0x93C5 :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_5x5x5_OES = 0x93C6 :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_6x5x5_OES = 0x93C7 :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_6x6x5_OES = 0x93C8 :: GlEnum
pattern GL_COMPRESSED_RGBA_ASTC_6x6x6_OES = 0x93C9 :: GlEnum

pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_3x3x3_OES = 0x93E0 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x3x3_OES = 0x93E1 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4x3_OES = 0x93E2 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4x4_OES = 0x93E3 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4x4_OES = 0x93E4 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5x4_OES = 0x93E5 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5x5_OES = 0x93E6 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5x5_OES = 0x93E7 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6x5_OES = 0x93E8 :: GlEnum
pattern GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6x6_OES = 0x93E9 :: GlEnum

--
-- ATC
--

pattern GL_ATC_RGB_AMD = 0x8C92 :: GlEnum
pattern GL_ATC_RGBA_EXPLICIT_ALPHA_AMD = 0x8C93 :: GlEnum
pattern GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD = 0x87EE :: GlEnum

--
-- Palletized (combined palette)
--

pattern GL_PALETTE4_RGB8_OES = 0x8B90 :: GlEnum
pattern GL_PALETTE4_RGBA8_OES = 0x8B91 :: GlEnum
pattern GL_PALETTE4_R5_G6_B5_OES = 0x8B92 :: GlEnum
pattern GL_PALETTE4_RGBA4_OES = 0x8B93 :: GlEnum
pattern GL_PALETTE4_RGB5_A1_OES = 0x8B94 :: GlEnum
pattern GL_PALETTE8_RGB8_OES = 0x8B95 :: GlEnum
pattern GL_PALETTE8_RGBA8_OES = 0x8B96 :: GlEnum
pattern GL_PALETTE8_R5_G6_B5_OES = 0x8B97 :: GlEnum
pattern GL_PALETTE8_RGBA4_OES = 0x8B98 :: GlEnum
pattern GL_PALETTE8_RGB5_A1_OES = 0x8B99 :: GlEnum

--
-- Palletized (separate palette)
--

pattern GL_COLOR_INDEX1_EXT = 0x80E2 :: GlEnum -- deprecated
pattern GL_COLOR_INDEX2_EXT = 0x80E3 :: GlEnum -- deprecated
pattern GL_COLOR_INDEX4_EXT = 0x80E4 :: GlEnum -- deprecated
pattern GL_COLOR_INDEX8_EXT = 0x80E5 :: GlEnum -- deprecated
pattern GL_COLOR_INDEX12_EXT = 0x80E6 :: GlEnum -- deprecated
pattern GL_COLOR_INDEX16_EXT = 0x80E7 :: GlEnum -- deprecated

--
-- Depth/stencil
--

pattern GL_DEPTH_COMPONENT16 = 0x81A5 :: GlEnum -- same as GL_DEPTH_COMPONENT16_SGIX and GL_DEPTH_COMPONENT16_ARB
pattern GL_DEPTH_COMPONENT24 = 0x81A6 :: GlEnum -- same as GL_DEPTH_COMPONENT24_SGIX and GL_DEPTH_COMPONENT24_ARB
pattern GL_DEPTH_COMPONENT32 = 0x81A7 :: GlEnum -- same as GL_DEPTH_COMPONENT32_SGIX and GL_DEPTH_COMPONENT32_ARB and GL_DEPTH_COMPONENT32_OES
pattern GL_DEPTH_COMPONENT32F = 0x8CAC :: GlEnum -- same as GL_DEPTH_COMPONENT32F_ARB
pattern GL_DEPTH_COMPONENT32F_NV = 0x8DAB :: GlEnum -- note that this is different from GL_DEPTH_COMPONENT32F
pattern GL_STENCIL_INDEX1 = 0x8D46 :: GlEnum -- same as GL_STENCIL_INDEX1_EXT
pattern GL_STENCIL_INDEX4 = 0x8D47 :: GlEnum -- same as GL_STENCIL_INDEX4_EXT
pattern GL_STENCIL_INDEX8 = 0x8D48 :: GlEnum -- same as GL_STENCIL_INDEX8_EXT
pattern GL_STENCIL_INDEX16 = 0x8D49 :: GlEnum -- same as GL_STENCIL_INDEX16_EXT
pattern GL_DEPTH24_STENCIL8 = 0x88F0 :: GlEnum -- same as GL_DEPTH24_STENCIL8_EXT and GL_DEPTH24_STENCIL8_OES
pattern GL_DEPTH32F_STENCIL8 = 0x8CAD :: GlEnum -- same as GL_DEPTH32F_STENCIL8_ARB
pattern GL_DEPTH32F_STENCIL8_NV = 0x8DAC :: GlEnum -- note that this is different from GL_DEPTH32F_STENCIL8

glGetFormatFromInternalFormat :: GlEnum -> GlEnum
glGetFormatFromInternalFormat = \case
  --
  -- 8 bits per component
  --
  GL_R8 -> GL_RED -- 1-component, 8-bit unsigned normalized
  GL_RG8 -> GL_RG -- 2-component, 8-bit unsigned normalized
  GL_RGB8 -> GL_RGB -- 3-component, 8-bit unsigned normalized
  GL_RGBA8 -> GL_RGBA -- 4-component, 8-bit unsigned normalized

  GL_R8_SNORM -> GL_RED -- 1-component, 8-bit signed normalized
  GL_RG8_SNORM -> GL_RG -- 2-component, 8-bit signed normalized
  GL_RGB8_SNORM -> GL_RGB -- 3-component, 8-bit signed normalized
  GL_RGBA8_SNORM -> GL_RGBA -- 4-component, 8-bit signed normalized

  GL_R8UI -> GL_RED -- 1-component, 8-bit unsigned integer
  GL_RG8UI -> GL_RG -- 2-component, 8-bit unsigned integer
  GL_RGB8UI -> GL_RGB -- 3-component, 8-bit unsigned integer
  GL_RGBA8UI -> GL_RGBA -- 4-component, 8-bit unsigned integer

  GL_R8I -> GL_RED -- 1-component, 8-bit signed integer
  GL_RG8I -> GL_RG -- 2-component, 8-bit signed integer
  GL_RGB8I -> GL_RGB -- 3-component, 8-bit signed integer
  GL_RGBA8I -> GL_RGBA -- 4-component, 8-bit signed integer

  GL_SR8 -> GL_RED -- 1-component, 8-bit sRGB
  GL_SRG8 -> GL_RG -- 2-component, 8-bit sRGB
  GL_SRGB8 -> GL_RGB -- 3-component, 8-bit sRGB
  GL_SRGB8_ALPHA8 -> GL_RGBA -- 4-component, 8-bit sRGB

  --
  -- 16 bits per component
  --
  GL_R16 -> GL_RED -- 1-component, 16-bit unsigned normalized
  GL_RG16 -> GL_RG -- 2-component, 16-bit unsigned normalized
  GL_RGB16 -> GL_RGB -- 3-component, 16-bit unsigned normalized
  GL_RGBA16 -> GL_RGBA -- 4-component, 16-bit unsigned normalized

  GL_R16_SNORM -> GL_RED -- 1-component, 16-bit signed normalized
  GL_RG16_SNORM -> GL_RG -- 2-component, 16-bit signed normalized
  GL_RGB16_SNORM -> GL_RGB -- 3-component, 16-bit signed normalized
  GL_RGBA16_SNORM -> GL_RGBA -- 4-component, 16-bit signed normalized

  GL_R16UI -> GL_RED -- 1-component, 16-bit unsigned integer
  GL_RG16UI -> GL_RG -- 2-component, 16-bit unsigned integer
  GL_RGB16UI -> GL_RGB -- 3-component, 16-bit unsigned integer
  GL_RGBA16UI -> GL_RGBA -- 4-component, 16-bit unsigned integer

  GL_R16I -> GL_RED -- 1-component, 16-bit signed integer
  GL_RG16I -> GL_RG -- 2-component, 16-bit signed integer
  GL_RGB16I -> GL_RGB -- 3-component, 16-bit signed integer
  GL_RGBA16I -> GL_RGBA -- 4-component, 16-bit signed integer

  GL_R16F -> GL_RED -- 1-component, 16-bit floating-point
  GL_RG16F -> GL_RG -- 2-component, 16-bit floating-point
  GL_RGB16F -> GL_RGB -- 3-component, 16-bit floating-point
  GL_RGBA16F -> GL_RGBA -- 4-component, 16-bit floating-point

  --
  -- 32 bits per component
  --
  GL_R32UI -> GL_RED -- 1-component, 32-bit unsigned integer
  GL_RG32UI -> GL_RG -- 2-component, 32-bit unsigned integer
  GL_RGB32UI -> GL_RGB -- 3-component, 32-bit unsigned integer
  GL_RGBA32UI -> GL_RGBA -- 4-component, 32-bit unsigned integer

  GL_R32I -> GL_RED -- 1-component, 32-bit signed integer
  GL_RG32I -> GL_RG -- 2-component, 32-bit signed integer
  GL_RGB32I -> GL_RGB -- 3-component, 32-bit signed integer
  GL_RGBA32I -> GL_RGBA -- 4-component, 32-bit signed integer

  GL_R32F -> GL_RED -- 1-component, 32-bit floating-point
  GL_RG32F -> GL_RG -- 2-component, 32-bit floating-point
  GL_RGB32F -> GL_RGB -- 3-component, 32-bit floating-point
  GL_RGBA32F -> GL_RGBA -- 4-component, 32-bit floating-point

  --
  -- Packed
  --
  GL_R3_G3_B2 -> GL_RGB -- 3-component 3:3:2,       unsigned normalized
  GL_RGB4 -> GL_RGB -- 3-component 4:4:4,       unsigned normalized
  GL_RGB5 -> GL_RGB -- 3-component 5:5:5,       unsigned normalized
  GL_RGB565 -> GL_RGB -- 3-component 5:6:5,       unsigned normalized
  GL_RGB10 -> GL_RGB -- 3-component 10:10:10,    unsigned normalized
  GL_RGB12 -> GL_RGB -- 3-component 12:12:12,    unsigned normalized
  GL_RGBA2 -> GL_RGBA -- 4-component 2:2:2:2,     unsigned normalized
  GL_RGBA4 -> GL_RGBA -- 4-component 4:4:4:4,     unsigned normalized
  GL_RGBA12 -> GL_RGBA -- 4-component 12:12:12:12, unsigned normalized
  GL_RGB5_A1 -> GL_RGBA -- 4-component 5:5:5:1,     unsigned normalized
  GL_RGB10_A2 -> GL_RGBA -- 4-component 10:10:10:2,  unsigned normalized
  GL_RGB10_A2UI -> GL_RGBA -- 4-component 10:10:10:2,  unsigned integer
  GL_R11F_G11F_B10F -> GL_RGB -- 3-component 11:11:10,    floating-point
  GL_RGB9_E5 -> GL_RGB -- 3-component/exp 9:9:9/5, floating-point

  --
  -- S3TC/DXT/BC
  --

  GL_COMPRESSED_RGB_S3TC_DXT1_EXT -> GL_RGB -- line through 3D space, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT -> GL_RGBA -- line through 3D space plus 1-bit alpha, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT -> GL_RGBA -- line through 3D space plus line through 1D space, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT -> GL_RGBA -- line through 3D space plus 4-bit alpha, 4x4 blocks, unsigned normalized

  GL_COMPRESSED_SRGB_S3TC_DXT1_EXT -> GL_RGB -- line through 3D space, 4x4 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT -> GL_RGBA -- line through 3D space plus 1-bit alpha, 4x4 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT -> GL_RGBA -- line through 3D space plus line through 1D space, 4x4 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT -> GL_RGBA -- line through 3D space plus 4-bit alpha, 4x4 blocks, sRGB

  GL_COMPRESSED_LUMINANCE_LATC1_EXT -> GL_RED -- line through 1D space, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT -> GL_RG -- two lines through 1D space, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT -> GL_RED -- line through 1D space, 4x4 blocks, signed normalized
  GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT -> GL_RG -- two lines through 1D space, 4x4 blocks, signed normalized

  GL_COMPRESSED_RED_RGTC1 -> GL_RED -- line through 1D space, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RG_RGTC2 -> GL_RG -- two lines through 1D space, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_SIGNED_RED_RGTC1 -> GL_RED -- line through 1D space, 4x4 blocks, signed normalized
  GL_COMPRESSED_SIGNED_RG_RGTC2 -> GL_RG -- two lines through 1D space, 4x4 blocks, signed normalized

  GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT -> GL_RGB -- 3-component, 4x4 blocks, unsigned floating-point
  GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT -> GL_RGB -- 3-component, 4x4 blocks, signed floating-point
  GL_COMPRESSED_RGBA_BPTC_UNORM -> GL_RGBA -- 4-component, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM -> GL_RGBA -- 4-component, 4x4 blocks, sRGB

  --
  -- ETC
  --
  GL_ETC1_RGB8_OES -> GL_RGB -- 3-component ETC1, 4x4 blocks, unsigned normalized

  GL_COMPRESSED_RGB8_ETC2 -> GL_RGB -- 3-component ETC2, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2 -> GL_RGBA -- 4-component ETC2 with 1-bit alpha, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA8_ETC2_EAC -> GL_RGBA -- 4-component ETC2, 4x4 blocks, unsigned normalized

  GL_COMPRESSED_SRGB8_ETC2 -> GL_RGB -- 3-component ETC2, 4x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2 -> GL_RGBA -- 4-component ETC2 with 1-bit alpha, 4x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC -> GL_RGBA -- 4-component ETC2, 4x4 blocks, sRGB

  GL_COMPRESSED_R11_EAC -> GL_RED -- 1-component ETC, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RG11_EAC -> GL_RG -- 2-component ETC, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_SIGNED_R11_EAC -> GL_RED -- 1-component ETC, 4x4 blocks, signed normalized
  GL_COMPRESSED_SIGNED_RG11_EAC -> GL_RG -- 2-component ETC, 4x4 blocks, signed normalized

  --
  -- PVRTC
  --
  GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG -> GL_RGB -- 3-component PVRTC, 16x8 blocks, unsigned normalized
  GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG -> GL_RGB -- 3-component PVRTC,  8x8 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG -> GL_RGBA -- 4-component PVRTC, 16x8 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG -> GL_RGBA -- 4-component PVRTC,  8x8 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_PVRTC_2BPPV2_IMG -> GL_RGBA -- 4-component PVRTC,  8x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_PVRTC_4BPPV2_IMG -> GL_RGBA -- 4-component PVRTC,  4x4 blocks, unsigned normalized

  GL_COMPRESSED_SRGB_PVRTC_2BPPV1_EXT -> GL_RGB -- 3-component PVRTC, 16x8 blocks, sRGB
  GL_COMPRESSED_SRGB_PVRTC_4BPPV1_EXT -> GL_RGB -- 3-component PVRTC,  8x8 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_PVRTC_2BPPV1_EXT -> GL_RGBA -- 4-component PVRTC, 16x8 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_PVRTC_4BPPV1_EXT -> GL_RGBA -- 4-component PVRTC,  8x8 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_PVRTC_2BPPV2_IMG -> GL_RGBA -- 4-component PVRTC,  8x4 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_PVRTC_4BPPV2_IMG -> GL_RGBA -- 4-component PVRTC,  4x4 blocks, sRGB

  --
  -- ASTC
  --
  GL_COMPRESSED_RGBA_ASTC_4x4_KHR -> GL_RGBA -- 4-component ASTC, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_5x4_KHR -> GL_RGBA -- 4-component ASTC, 5x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_5x5_KHR -> GL_RGBA -- 4-component ASTC, 5x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_6x5_KHR -> GL_RGBA -- 4-component ASTC, 6x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_6x6_KHR -> GL_RGBA -- 4-component ASTC, 6x6 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_8x5_KHR -> GL_RGBA -- 4-component ASTC, 8x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_8x6_KHR -> GL_RGBA -- 4-component ASTC, 8x6 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_8x8_KHR -> GL_RGBA -- 4-component ASTC, 8x8 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_10x5_KHR -> GL_RGBA -- 4-component ASTC, 10x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_10x6_KHR -> GL_RGBA -- 4-component ASTC, 10x6 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_10x8_KHR -> GL_RGBA -- 4-component ASTC, 10x8 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_10x10_KHR -> GL_RGBA -- 4-component ASTC, 10x10 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_12x10_KHR -> GL_RGBA -- 4-component ASTC, 12x10 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_12x12_KHR -> GL_RGBA -- 4-component ASTC, 12x12 blocks, unsigned normalized

  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR -> GL_RGBA -- 4-component ASTC, 4x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR -> GL_RGBA -- 4-component ASTC, 5x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR -> GL_RGBA -- 4-component ASTC, 5x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR -> GL_RGBA -- 4-component ASTC, 6x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR -> GL_RGBA -- 4-component ASTC, 6x6 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR -> GL_RGBA -- 4-component ASTC, 8x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR -> GL_RGBA -- 4-component ASTC, 8x6 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR -> GL_RGBA -- 4-component ASTC, 8x8 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR -> GL_RGBA -- 4-component ASTC, 10x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR -> GL_RGBA -- 4-component ASTC, 10x6 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR -> GL_RGBA -- 4-component ASTC, 10x8 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR -> GL_RGBA -- 4-component ASTC, 10x10 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR -> GL_RGBA -- 4-component ASTC, 12x10 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR -> GL_RGBA -- 4-component ASTC, 12x12 blocks, sRGB

  GL_COMPRESSED_RGBA_ASTC_3x3x3_OES -> GL_RGBA -- 4-component ASTC, 3x3x3 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_4x3x3_OES -> GL_RGBA -- 4-component ASTC, 4x3x3 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_4x4x3_OES -> GL_RGBA -- 4-component ASTC, 4x4x3 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_4x4x4_OES -> GL_RGBA -- 4-component ASTC, 4x4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_5x4x4_OES -> GL_RGBA -- 4-component ASTC, 5x4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_5x5x4_OES -> GL_RGBA -- 4-component ASTC, 5x5x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_5x5x5_OES -> GL_RGBA -- 4-component ASTC, 5x5x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_6x5x5_OES -> GL_RGBA -- 4-component ASTC, 6x5x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_6x6x5_OES -> GL_RGBA -- 4-component ASTC, 6x6x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_6x6x6_OES -> GL_RGBA -- 4-component ASTC, 6x6x6 blocks, unsigned normalized

  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_3x3x3_OES -> GL_RGBA -- 4-component ASTC, 3x3x3 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x3x3_OES -> GL_RGBA -- 4-component ASTC, 4x3x3 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4x3_OES -> GL_RGBA -- 4-component ASTC, 4x4x3 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4x4_OES -> GL_RGBA -- 4-component ASTC, 4x4x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4x4_OES -> GL_RGBA -- 4-component ASTC, 5x4x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5x4_OES -> GL_RGBA -- 4-component ASTC, 5x5x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5x5_OES -> GL_RGBA -- 4-component ASTC, 5x5x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5x5_OES -> GL_RGBA -- 4-component ASTC, 6x5x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6x5_OES -> GL_RGBA -- 4-component ASTC, 6x6x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6x6_OES -> GL_RGBA -- 4-component ASTC, 6x6x6 blocks, sRGB

  --
  -- ATC
  --
  GL_ATC_RGB_AMD -> GL_RGB -- 3-component, 4x4 blocks, unsigned normalized
  GL_ATC_RGBA_EXPLICIT_ALPHA_AMD -> GL_RGBA -- 4-component, 4x4 blocks, unsigned normalized
  GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD -> GL_RGBA -- 4-component, 4x4 blocks, unsigned normalized

  --
  -- Palletized
  --
  GL_PALETTE4_RGB8_OES -> GL_RGB -- 3-component 8:8:8,   4-bit palette, unsigned normalized
  GL_PALETTE4_RGBA8_OES -> GL_RGBA -- 4-component 8:8:8:8, 4-bit palette, unsigned normalized
  GL_PALETTE4_R5_G6_B5_OES -> GL_RGB -- 3-component 5:6:5,   4-bit palette, unsigned normalized
  GL_PALETTE4_RGBA4_OES -> GL_RGBA -- 4-component 4:4:4:4, 4-bit palette, unsigned normalized
  GL_PALETTE4_RGB5_A1_OES -> GL_RGBA -- 4-component 5:5:5:1, 4-bit palette, unsigned normalized
  GL_PALETTE8_RGB8_OES -> GL_RGB -- 3-component 8:8:8,   8-bit palette, unsigned normalized
  GL_PALETTE8_RGBA8_OES -> GL_RGBA -- 4-component 8:8:8:8, 8-bit palette, unsigned normalized
  GL_PALETTE8_R5_G6_B5_OES -> GL_RGB -- 3-component 5:6:5,   8-bit palette, unsigned normalized
  GL_PALETTE8_RGBA4_OES -> GL_RGBA -- 4-component 4:4:4:4, 8-bit palette, unsigned normalized
  GL_PALETTE8_RGB5_A1_OES -> GL_RGBA -- 4-component 5:5:5:1, 8-bit palette, unsigned normalized

  --
  -- Depth/stencil
  --
  GL_DEPTH_COMPONENT16 -> GL_DEPTH_COMPONENT
  GL_DEPTH_COMPONENT24 -> GL_DEPTH_COMPONENT
  GL_DEPTH_COMPONENT32 -> GL_DEPTH_COMPONENT
  GL_DEPTH_COMPONENT32F -> GL_DEPTH_COMPONENT
  GL_DEPTH_COMPONENT32F_NV -> GL_DEPTH_COMPONENT
  GL_STENCIL_INDEX1 -> GL_STENCIL_INDEX
  GL_STENCIL_INDEX4 -> GL_STENCIL_INDEX
  GL_STENCIL_INDEX8 -> GL_STENCIL_INDEX
  GL_STENCIL_INDEX16 -> GL_STENCIL_INDEX
  GL_DEPTH24_STENCIL8 -> GL_DEPTH_STENCIL
  GL_DEPTH32F_STENCIL8 -> GL_DEPTH_STENCIL
  GL_DEPTH32F_STENCIL8_NV -> GL_DEPTH_STENCIL

  v -> throwGlConstantException $ "Invalid GL internal format: " ++ show v

glGetTypeFromInternalFormat :: GlEnum -> GlEnum
glGetTypeFromInternalFormat = \case
  --
  -- 8 bits per component
  --
  GL_R8 -> GL_UNSIGNED_BYTE -- 1-component, 8-bit unsigned normalized
  GL_RG8 -> GL_UNSIGNED_BYTE -- 2-component, 8-bit unsigned normalized
  GL_RGB8 -> GL_UNSIGNED_BYTE -- 3-component, 8-bit unsigned normalized
  GL_RGBA8 -> GL_UNSIGNED_BYTE -- 4-component, 8-bit unsigned normalized

  GL_R8_SNORM -> GL_BYTE -- 1-component, 8-bit signed normalized
  GL_RG8_SNORM -> GL_BYTE -- 2-component, 8-bit signed normalized
  GL_RGB8_SNORM -> GL_BYTE -- 3-component, 8-bit signed normalized
  GL_RGBA8_SNORM -> GL_BYTE -- 4-component, 8-bit signed normalized

  GL_R8UI -> GL_UNSIGNED_BYTE -- 1-component, 8-bit unsigned integer
  GL_RG8UI -> GL_UNSIGNED_BYTE -- 2-component, 8-bit unsigned integer
  GL_RGB8UI -> GL_UNSIGNED_BYTE -- 3-component, 8-bit unsigned integer
  GL_RGBA8UI -> GL_UNSIGNED_BYTE -- 4-component, 8-bit unsigned integer

  GL_R8I -> GL_BYTE -- 1-component, 8-bit signed integer
  GL_RG8I -> GL_BYTE -- 2-component, 8-bit signed integer
  GL_RGB8I -> GL_BYTE -- 3-component, 8-bit signed integer
  GL_RGBA8I -> GL_BYTE -- 4-component, 8-bit signed integer

  GL_SR8 -> GL_UNSIGNED_BYTE -- 1-component, 8-bit sRGB
  GL_SRG8 -> GL_UNSIGNED_BYTE -- 2-component, 8-bit sRGB
  GL_SRGB8 -> GL_UNSIGNED_BYTE -- 3-component, 8-bit sRGB
  GL_SRGB8_ALPHA8 -> GL_UNSIGNED_BYTE -- 4-component, 8-bit sRGB

  --
  -- 16 bits per component
  --
  GL_R16 -> GL_UNSIGNED_SHORT -- 1-component, 16-bit unsigned normalized
  GL_RG16 -> GL_UNSIGNED_SHORT -- 2-component, 16-bit unsigned normalized
  GL_RGB16 -> GL_UNSIGNED_SHORT -- 3-component, 16-bit unsigned normalized
  GL_RGBA16 -> GL_UNSIGNED_SHORT -- 4-component, 16-bit unsigned normalized

  GL_R16_SNORM -> GL_SHORT -- 1-component, 16-bit signed normalized
  GL_RG16_SNORM -> GL_SHORT -- 2-component, 16-bit signed normalized
  GL_RGB16_SNORM -> GL_SHORT -- 3-component, 16-bit signed normalized
  GL_RGBA16_SNORM -> GL_SHORT -- 4-component, 16-bit signed normalized

  GL_R16UI -> GL_UNSIGNED_SHORT -- 1-component, 16-bit unsigned integer
  GL_RG16UI -> GL_UNSIGNED_SHORT -- 2-component, 16-bit unsigned integer
  GL_RGB16UI -> GL_UNSIGNED_SHORT -- 3-component, 16-bit unsigned integer
  GL_RGBA16UI -> GL_UNSIGNED_SHORT -- 4-component, 16-bit unsigned integer

  GL_R16I -> GL_SHORT -- 1-component, 16-bit signed integer
  GL_RG16I -> GL_SHORT -- 2-component, 16-bit signed integer
  GL_RGB16I -> GL_SHORT -- 3-component, 16-bit signed integer
  GL_RGBA16I -> GL_SHORT -- 4-component, 16-bit signed integer

  GL_R16F -> GL_HALF_FLOAT -- 1-component, 16-bit floating-point
  GL_RG16F -> GL_HALF_FLOAT -- 2-component, 16-bit floating-point
  GL_RGB16F -> GL_HALF_FLOAT -- 3-component, 16-bit floating-point
  GL_RGBA16F -> GL_HALF_FLOAT -- 4-component, 16-bit floating-point

  --
  -- 32 bits per component
  --
  GL_R32UI -> GL_UNSIGNED_INT -- 1-component, 32-bit unsigned integer
  GL_RG32UI -> GL_UNSIGNED_INT -- 2-component, 32-bit unsigned integer
  GL_RGB32UI -> GL_UNSIGNED_INT -- 3-component, 32-bit unsigned integer
  GL_RGBA32UI -> GL_UNSIGNED_INT -- 4-component, 32-bit unsigned integer

  GL_R32I -> GL_INT -- 1-component, 32-bit signed integer
  GL_RG32I -> GL_INT -- 2-component, 32-bit signed integer
  GL_RGB32I -> GL_INT -- 3-component, 32-bit signed integer
  GL_RGBA32I -> GL_INT -- 4-component, 32-bit signed integer

  GL_R32F -> GL_FLOAT -- 1-component, 32-bit floating-point
  GL_RG32F -> GL_FLOAT -- 2-component, 32-bit floating-point
  GL_RGB32F -> GL_FLOAT -- 3-component, 32-bit floating-point
  GL_RGBA32F -> GL_FLOAT -- 4-component, 32-bit floating-point

  --
  -- Packed
  --
  GL_R3_G3_B2 -> GL_UNSIGNED_BYTE_2_3_3_REV -- 3-component 3:3:2,       unsigned normalized
  GL_RGB4 -> GL_UNSIGNED_SHORT_4_4_4_4 -- 3-component 4:4:4,       unsigned normalized
  GL_RGB5 -> GL_UNSIGNED_SHORT_5_5_5_1 -- 3-component 5:5:5,       unsigned normalized
  GL_RGB565 -> GL_UNSIGNED_SHORT_5_6_5 -- 3-component 5:6:5,       unsigned normalized
  GL_RGB10 -> GL_UNSIGNED_INT_10_10_10_2 -- 3-component 10:10:10,    unsigned normalized
  GL_RGB12 -> GL_UNSIGNED_SHORT -- 3-component 12:12:12,    unsigned normalized
  GL_RGBA2 -> GL_UNSIGNED_BYTE -- 4-component 2:2:2:2,     unsigned normalized
  GL_RGBA4 -> GL_UNSIGNED_SHORT_4_4_4_4 -- 4-component 4:4:4:4,     unsigned normalized
  GL_RGBA12 -> GL_UNSIGNED_SHORT -- 4-component 12:12:12:12, unsigned normalized
  GL_RGB5_A1 -> GL_UNSIGNED_SHORT_5_5_5_1 -- 4-component 5:5:5:1,     unsigned normalized
  GL_RGB10_A2 -> GL_UNSIGNED_INT_2_10_10_10_REV -- 4-component 10:10:10:2,  unsigned normalized
  GL_RGB10_A2UI -> GL_UNSIGNED_INT_2_10_10_10_REV -- 4-component 10:10:10:2,  unsigned integer
  GL_R11F_G11F_B10F -> GL_UNSIGNED_INT_10F_11F_11F_REV -- 3-component 11:11:10,    floating-point
  GL_RGB9_E5 -> GL_UNSIGNED_INT_5_9_9_9_REV -- 3-component/exp 9:9:9/5, floating-point

  --
  -- S3TC/DXT/BC
  --

  GL_COMPRESSED_RGB_S3TC_DXT1_EXT -> GL_UNSIGNED_BYTE -- line through 3D space, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT -> GL_UNSIGNED_BYTE -- line through 3D space plus 1-bit alpha, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT -> GL_UNSIGNED_BYTE -- line through 3D space plus line through 1D space, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT -> GL_UNSIGNED_BYTE -- line through 3D space plus 4-bit alpha, 4x4 blocks, unsigned normalized

  GL_COMPRESSED_SRGB_S3TC_DXT1_EXT -> GL_UNSIGNED_BYTE -- line through 3D space, 4x4 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT -> GL_UNSIGNED_BYTE -- line through 3D space plus 1-bit alpha, 4x4 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT -> GL_UNSIGNED_BYTE -- line through 3D space plus line through 1D space, 4x4 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT -> GL_UNSIGNED_BYTE -- line through 3D space plus 4-bit alpha, 4x4 blocks, sRGB

  GL_COMPRESSED_LUMINANCE_LATC1_EXT -> GL_UNSIGNED_BYTE -- line through 1D space, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT -> GL_UNSIGNED_BYTE -- two lines through 1D space, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT -> GL_UNSIGNED_BYTE -- line through 1D space, 4x4 blocks, signed normalized
  GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT -> GL_UNSIGNED_BYTE -- two lines through 1D space, 4x4 blocks, signed normalized

  GL_COMPRESSED_RED_RGTC1 -> GL_UNSIGNED_BYTE -- line through 1D space, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RG_RGTC2 -> GL_UNSIGNED_BYTE -- two lines through 1D space, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_SIGNED_RED_RGTC1 -> GL_UNSIGNED_BYTE -- line through 1D space, 4x4 blocks, signed normalized
  GL_COMPRESSED_SIGNED_RG_RGTC2 -> GL_UNSIGNED_BYTE -- two lines through 1D space, 4x4 blocks, signed normalized

  GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT -> GL_FLOAT -- 3-component, 4x4 blocks, unsigned floating-point
  GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT -> GL_FLOAT -- 3-component, 4x4 blocks, signed floating-point
  GL_COMPRESSED_RGBA_BPTC_UNORM -> GL_UNSIGNED_BYTE -- 4-component, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM -> GL_UNSIGNED_BYTE -- 4-component, 4x4 blocks, sRGB

  --
  -- ETC
  --
  GL_ETC1_RGB8_OES -> GL_UNSIGNED_BYTE -- 3-component ETC1, 4x4 blocks, unsigned normalized" ),

  GL_COMPRESSED_RGB8_ETC2 -> GL_UNSIGNED_BYTE -- 3-component ETC2, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2 -> GL_UNSIGNED_BYTE -- 4-component ETC2 with 1-bit alpha, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA8_ETC2_EAC -> GL_UNSIGNED_BYTE -- 4-component ETC2, 4x4 blocks, unsigned normalized

  GL_COMPRESSED_SRGB8_ETC2 -> GL_UNSIGNED_BYTE -- 3-component ETC2, 4x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2 -> GL_UNSIGNED_BYTE -- 4-component ETC2 with 1-bit alpha, 4x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC -> GL_UNSIGNED_BYTE -- 4-component ETC2, 4x4 blocks, sRGB

  GL_COMPRESSED_R11_EAC -> GL_UNSIGNED_BYTE -- 1-component ETC, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RG11_EAC -> GL_UNSIGNED_BYTE -- 2-component ETC, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_SIGNED_R11_EAC -> GL_UNSIGNED_BYTE -- 1-component ETC, 4x4 blocks, signed normalized
  GL_COMPRESSED_SIGNED_RG11_EAC -> GL_UNSIGNED_BYTE -- 2-component ETC, 4x4 blocks, signed normalized

  --
  -- PVRTC
  --
  GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG -> GL_UNSIGNED_BYTE -- 3-component PVRTC, 16x8 blocks, unsigned normalized
  GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG -> GL_UNSIGNED_BYTE -- 3-component PVRTC,  8x8 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG -> GL_UNSIGNED_BYTE -- 4-component PVRTC, 16x8 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG -> GL_UNSIGNED_BYTE -- 4-component PVRTC,  8x8 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_PVRTC_2BPPV2_IMG -> GL_UNSIGNED_BYTE -- 4-component PVRTC,  8x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_PVRTC_4BPPV2_IMG -> GL_UNSIGNED_BYTE -- 4-component PVRTC,  4x4 blocks, unsigned normalized

  GL_COMPRESSED_SRGB_PVRTC_2BPPV1_EXT -> GL_UNSIGNED_BYTE -- 3-component PVRTC, 16x8 blocks, sRGB
  GL_COMPRESSED_SRGB_PVRTC_4BPPV1_EXT -> GL_UNSIGNED_BYTE -- 3-component PVRTC,  8x8 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_PVRTC_2BPPV1_EXT -> GL_UNSIGNED_BYTE -- 4-component PVRTC, 16x8 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_PVRTC_4BPPV1_EXT -> GL_UNSIGNED_BYTE -- 4-component PVRTC,  8x8 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_PVRTC_2BPPV2_IMG -> GL_UNSIGNED_BYTE -- 4-component PVRTC,  8x4 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_PVRTC_4BPPV2_IMG -> GL_UNSIGNED_BYTE -- 4-component PVRTC,  4x4 blocks, sRGB

  --
  -- ASTC
  --
  GL_COMPRESSED_RGBA_ASTC_4x4_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_5x4_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 5x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_5x5_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 5x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_6x5_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 6x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_6x6_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 6x6 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_8x5_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 8x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_8x6_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 8x6 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_8x8_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 8x8 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_10x5_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 10x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_10x6_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 10x6 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_10x8_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 10x8 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_10x10_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 10x10 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_12x10_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 12x10 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_12x12_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 12x12 blocks, unsigned normalized

  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 4x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 5x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 5x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 6x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 6x6 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 8x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 8x6 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 8x8 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 10x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 10x6 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 10x8 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 10x10 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 12x10 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR -> GL_UNSIGNED_BYTE -- 4-component ASTC, 12x12 blocks, sRGB

  GL_COMPRESSED_RGBA_ASTC_3x3x3_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 3x3x3 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_4x3x3_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 4x3x3 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_4x4x3_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 4x4x3 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_4x4x4_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 4x4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_5x4x4_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 5x4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_5x5x4_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 5x5x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_5x5x5_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 5x5x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_6x5x5_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 6x5x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_6x6x5_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 6x6x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_6x6x6_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 6x6x6 blocks, unsigned normalized

  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_3x3x3_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 3x3x3 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x3x3_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 4x3x3 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4x3_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 4x4x3 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4x4_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 4x4x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4x4_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 5x4x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5x4_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 5x5x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5x5_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 5x5x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5x5_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 6x5x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6x5_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 6x6x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6x6_OES -> GL_UNSIGNED_BYTE -- 4-component ASTC, 6x6x6 blocks, sRGB

  --
  -- ATC
  --
  GL_ATC_RGB_AMD -> GL_UNSIGNED_BYTE -- 3-component, 4x4 blocks, unsigned normalized
  GL_ATC_RGBA_EXPLICIT_ALPHA_AMD -> GL_UNSIGNED_BYTE -- 4-component, 4x4 blocks, unsigned normalized
  GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD -> GL_UNSIGNED_BYTE -- 4-component, 4x4 blocks, unsigned normalized

  --
  -- Palletized
  --
  GL_PALETTE4_RGB8_OES -> GL_UNSIGNED_BYTE -- 3-component 8:8:8,   4-bit palette, unsigned normalized
  GL_PALETTE4_RGBA8_OES -> GL_UNSIGNED_BYTE -- 4-component 8:8:8:8, 4-bit palette, unsigned normalized
  GL_PALETTE4_R5_G6_B5_OES -> GL_UNSIGNED_SHORT_5_6_5 -- 3-component 5:6:5,   4-bit palette, unsigned normalized
  GL_PALETTE4_RGBA4_OES -> GL_UNSIGNED_SHORT_4_4_4_4 -- 4-component 4:4:4:4, 4-bit palette, unsigned normalized
  GL_PALETTE4_RGB5_A1_OES -> GL_UNSIGNED_SHORT_5_5_5_1 -- 4-component 5:5:5:1, 4-bit palette, unsigned normalized
  GL_PALETTE8_RGB8_OES -> GL_UNSIGNED_BYTE -- 3-component 8:8:8,   8-bit palette, unsigned normalized
  GL_PALETTE8_RGBA8_OES -> GL_UNSIGNED_BYTE -- 4-component 8:8:8:8, 8-bit palette, unsigned normalized
  GL_PALETTE8_R5_G6_B5_OES -> GL_UNSIGNED_SHORT_5_6_5 -- 3-component 5:6:5,   8-bit palette, unsigned normalized
  GL_PALETTE8_RGBA4_OES -> GL_UNSIGNED_SHORT_4_4_4_4 -- 4-component 4:4:4:4, 8-bit palette, unsigned normalized
  GL_PALETTE8_RGB5_A1_OES -> GL_UNSIGNED_SHORT_5_5_5_1 -- 4-component 5:5:5:1, 8-bit palette, unsigned normalized

  --
  -- Depth/stencil
  --
  GL_DEPTH_COMPONENT16 -> GL_UNSIGNED_SHORT
  GL_DEPTH_COMPONENT24 -> GL_UNSIGNED_INT_24_8
  GL_DEPTH_COMPONENT32 -> GL_UNSIGNED_INT
  GL_DEPTH_COMPONENT32F -> GL_FLOAT
  GL_DEPTH_COMPONENT32F_NV -> GL_FLOAT
  GL_STENCIL_INDEX1 -> GL_UNSIGNED_BYTE
  GL_STENCIL_INDEX4 -> GL_UNSIGNED_BYTE
  GL_STENCIL_INDEX8 -> GL_UNSIGNED_BYTE
  GL_STENCIL_INDEX16 -> GL_UNSIGNED_SHORT
  GL_DEPTH24_STENCIL8 -> GL_UNSIGNED_INT_24_8
  GL_DEPTH32F_STENCIL8 -> GL_FLOAT_32_UNSIGNED_INT_24_8_REV
  GL_DEPTH32F_STENCIL8_NV -> GL_FLOAT_32_UNSIGNED_INT_24_8_REV

  v -> throwGlConstantException $ "Invalid GL internal format: " ++ show v

glGetTypeSizeFromType :: GlEnum -> Word32
glGetTypeSizeFromType = \case
  x | x `elem` [
      GL_BYTE,
      GL_UNSIGNED_BYTE,
      GL_UNSIGNED_BYTE_3_3_2,
      GL_UNSIGNED_BYTE_2_3_3_REV
    ] -> 1

  x | x `elem` [
      GL_SHORT,
      GL_UNSIGNED_SHORT,
      GL_UNSIGNED_SHORT_5_6_5,
      GL_UNSIGNED_SHORT_4_4_4_4,
      GL_UNSIGNED_SHORT_5_5_5_1,
      GL_UNSIGNED_SHORT_5_6_5_REV,
      GL_UNSIGNED_SHORT_4_4_4_4_REV,
      GL_UNSIGNED_SHORT_1_5_5_5_REV,
      GL_HALF_FLOAT
    ] -> 2

  x | x `elem` [
      GL_INT,
      GL_UNSIGNED_INT,
      GL_UNSIGNED_INT_8_8_8_8,
      GL_UNSIGNED_INT_8_8_8_8_REV,
      GL_UNSIGNED_INT_10_10_10_2,
      GL_UNSIGNED_INT_2_10_10_10_REV,
      GL_UNSIGNED_INT_24_8,
      GL_UNSIGNED_INT_10F_11F_11F_REV,
      GL_UNSIGNED_INT_5_9_9_9_REV,
      GL_FLOAT,
      GL_FLOAT_32_UNSIGNED_INT_24_8_REV
    ] -> 4

  v -> throwGlConstantException $ "Invalid GL type: " ++ show v

type GlFormatSizeFlags = Word32

pattern GL_FORMAT_SIZE_PACKED_BIT = 0x01 :: GlFormatSizeFlags
pattern GL_FORMAT_SIZE_COMPRESSED_BIT = 0x02 :: GlFormatSizeFlags
pattern GL_FORMAT_SIZE_PALETTIZED_BIT = 0x04 :: GlFormatSizeFlags
pattern GL_FORMAT_SIZE_DEPTH_BIT = 0x08 :: GlFormatSizeFlags
pattern GL_FORMAT_SIZE_STENCIL_BIT = 0x10 :: GlFormatSizeFlags

data GlFormatSize =
  GlFormatSize {
    glFormatSize'flags :: GlFormatSizeFlags,
    glFormatSize'paletteSizeInBits :: Word32,
    glFormatSize'blockSizeInBits :: Word32,
    glFormatSize'blockWidth :: Word32,
    glFormatSize'blockHeight :: Word32,
    glFormatSize'blockDepth :: Word32
  }
  deriving (Show)

glGetFormatSize :: GlEnum -> GlFormatSize
glGetFormatSize = \case

  --
  -- 8 bits per component
  --

  x | x `elem` [
      GL_R8, -- 1-component, 8-bit unsigned normalized
      GL_R8_SNORM, -- 1-component, 8-bit signed normalized
      GL_R8UI, -- 1-component, 8-bit unsigned integer
      GL_R8I, -- 1-component, 8-bit signed integer
      GL_SR8 -- 1-component, 8-bit sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = 0,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 1 * 8,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_RG8, -- 2-component, 8-bit unsigned normalized
      GL_RG8_SNORM, -- 2-component, 8-bit signed normalized
      GL_RG8UI, -- 2-component, 8-bit unsigned integer
      GL_RG8I, -- 2-component, 8-bit signed integer
      GL_SRG8 -- 2-component, 8-bit sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = 0,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 2 * 8,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_RGB8, -- 3-component, 8-bit unsigned normalized
      GL_RGB8_SNORM, -- 3-component, 8-bit signed normalized
      GL_RGB8UI, -- 3-component, 8-bit unsigned integer
      GL_RGB8I, -- 3-component, 8-bit signed integer
      GL_SRGB8 -- 3-component, 8-bit sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = 0,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 3 * 8,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_RGBA8, -- 4-component, 8-bit unsigned normalized
      GL_RGBA8_SNORM, -- 4-component, 8-bit signed normalized
      GL_RGBA8UI, -- 4-component, 8-bit unsigned integer
      GL_RGBA8I, -- 4-component, 8-bit signed integer
      GL_SRGB8_ALPHA8 -- 4-component, 8-bit sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = 0,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 4 * 8,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  --
  -- 16 bits per component
  --

  x | x `elem` [
      GL_R16, -- 1-component, 16-bit unsigned normalized
      GL_R16_SNORM, -- 1-component, 16-bit signed normalized
      GL_R16UI, -- 1-component, 16-bit unsigned integer
      GL_R16I, -- 1-component, 16-bit signed integer
      GL_R16F -- 1-component, 16-bit floating-point
    ] ->
    GlFormatSize {
      glFormatSize'flags = 0,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 2 * 8,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_RG16, -- 2-component, 16-bit unsigned normalized
      GL_RG16_SNORM, -- 2-component, 16-bit signed normalized
      GL_RG16UI, -- 2-component, 16-bit unsigned integer
      GL_RG16I, -- 2-component, 16-bit signed integer
      GL_RG16F -- 2-component, 16-bit floating-point
    ] ->
    GlFormatSize {
      glFormatSize'flags = 0,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 4 * 8,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_RGB16, -- 3-component, 16-bit unsigned normalized
      GL_RGB16_SNORM, -- 3-component, 16-bit signed normalized
      GL_RGB16UI, -- 3-component, 16-bit unsigned integer
      GL_RGB16I, -- 3-component, 16-bit signed integer
      GL_RGB16F -- 3-component, 16-bit floating-point
    ] ->
    GlFormatSize {
      glFormatSize'flags = 0,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 6 * 8,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_RGBA16, -- 4-component, 16-bit unsigned normalized
      GL_RGBA16_SNORM, -- 4-component, 16-bit signed normalized
      GL_RGBA16UI, -- 4-component, 16-bit unsigned integer
      GL_RGBA16I, -- 4-component, 16-bit signed integer
      GL_RGBA16F -- 4-component, 16-bit floating-point
    ] ->
    GlFormatSize {
      glFormatSize'flags = 0,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 8 * 8,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  --
  -- 32 bits per component
  --

  x | x `elem` [
      GL_R32UI, -- 1-component, 32-bit unsigned integer
      GL_R32I, -- 1-component, 32-bit signed integer
      GL_R32F -- 1-component, 32-bit floating-point
    ] ->
    GlFormatSize {
      glFormatSize'flags = 0,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 4 * 8,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_RG32UI, -- 2-component, 32-bit unsigned integer
      GL_RG32I, -- 2-component, 32-bit signed integer
      GL_RG32F -- 2-component, 32-bit floating-point
    ] ->
    GlFormatSize {
      glFormatSize'flags = 0,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 8 * 8,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_RGB32UI, -- 3-component, 32-bit unsigned integer
      GL_RGB32I, -- 3-component, 32-bit signed integer
      GL_RGB32F -- 3-component, 32-bit floating-point
    ] ->
    GlFormatSize {
      glFormatSize'flags = 0,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 12 * 8,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_RGBA32UI, -- 4-component, 32-bit unsigned integer
      GL_RGBA32I, -- 4-component, 32-bit signed integer
      GL_RGBA32F -- 4-component, 32-bit floating-point
    ] ->
    GlFormatSize {
      glFormatSize'flags = 0,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 16 * 8,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  --
  -- Packed
  --

  GL_R3_G3_B2 -> -- 3-component 3,3,2, unsigned normalized
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_PACKED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 8,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  GL_RGB4 -> -- 3-component 4,4,4, unsigned normalized
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_PACKED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 12,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  GL_RGB5 -> -- 3-component 5,5,5, unsigned normalized
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_PACKED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 16,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  GL_RGB565 -> -- 3-component 5,6,5, unsigned normalized
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_PACKED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 16,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  GL_RGB10 -> -- 3-component 10,10,10, unsigned normalized
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_PACKED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 32,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  GL_RGB12 -> -- 3-component 12,12,12, unsigned normalized
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_PACKED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 36,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  GL_RGBA2 -> -- 4-component 2,2,2,2, unsigned normalized
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_PACKED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 8,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  GL_RGBA4 -> -- 4-component 4,4,4,4, unsigned normalized
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_PACKED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 16,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  GL_RGBA12 -> -- 4-component 12,12,12,12, unsigned normalized
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_PACKED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 48,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  GL_RGB5_A1 -> -- 4-component 5,5,5,1, unsigned normalized
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_PACKED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 32,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  GL_RGB10_A2 -> -- 4-component 10,10,10,2, unsigned normalized
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_PACKED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 32,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  GL_RGB10_A2UI -> -- 4-component 10,10,10,2, unsigned integer
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_PACKED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 32,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_R11F_G11F_B10F, -- 3-component 11,11,10, floating-point
      GL_RGB9_E5 -- 3-component/exp 9,9,9/5, floating-point
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_PACKED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 32,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  --
  -- S3TC/DXT/BC
  --

  x | x `elem` [
      GL_COMPRESSED_RGB_S3TC_DXT1_EXT, -- line through 3D space, 4x4 blocks, unsigned normalized
      GL_COMPRESSED_RGBA_S3TC_DXT1_EXT, -- line through 3D space plus 1-bit alpha, 4x4 blocks, unsigned normalized
      GL_COMPRESSED_SRGB_S3TC_DXT1_EXT, -- line through 3D space, 4x4 blocks, sRGB
      GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT -- line through 3D space plus 1-bit alpha, 4x4 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 64,
      glFormatSize'blockWidth = 4,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_S3TC_DXT5_EXT, -- line through 3D space plus line through 1D space, 4x4 blocks, unsigned normalized
      GL_COMPRESSED_RGBA_S3TC_DXT3_EXT, -- line through 3D space plus 4-bit alpha, 4x4 blocks, unsigned normalized
      GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT, -- line through 3D space plus line through 1D space, 4x4 blocks, sRGB
      GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT -- line through 3D space plus 4-bit alpha, 4x4 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 4,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_LUMINANCE_LATC1_EXT, -- line through 1D space, 4x4 blocks, unsigned normalized
      GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT -- line through 1D space, 4x4 blocks, signed normalized
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 64,
      glFormatSize'blockWidth = 4,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT, -- two lines through 1D space, 4x4 blocks, unsigned normalized
      GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT -- two lines through 1D space, 4x4 blocks, signed normalized
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 4,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RED_RGTC1, -- line through 1D space, 4x4 blocks, unsigned normalized
      GL_COMPRESSED_SIGNED_RED_RGTC1 -- line through 1D space, 4x4 blocks, signed normalized
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 64,
      glFormatSize'blockWidth = 4,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RG_RGTC2, -- two lines through 1D space, 4x4 blocks, unsigned normalized
      GL_COMPRESSED_SIGNED_RG_RGTC2 -- two lines through 1D space, 4x4 blocks, signed normalized
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 4,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT, -- 3-component, 4x4 blocks, unsigned floating-point
      GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT, -- 3-component, 4x4 blocks, signed floating-point
      GL_COMPRESSED_RGBA_BPTC_UNORM, -- 4-component, 4x4 blocks, unsigned normalized
      GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM -- 4-component, 4x4 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 4,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 1
    }

  --
  -- ETC
  --

  x | x `elem` [
      GL_ETC1_RGB8_OES, -- 3-component ETC1, 4x4 blocks, unsigned normalized" ),
      GL_COMPRESSED_RGB8_ETC2, -- 3-component ETC2, 4x4 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ETC2, -- 3-component ETC2, 4x4 blocks, sRGB
      GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2, -- 4-component ETC2 with 1-bit alpha, 4x4 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2 -- 4-component ETC2 with 1-bit alpha, 4x4 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 64,
      glFormatSize'blockWidth = 4,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA8_ETC2_EAC, -- 4-component ETC2, 4x4 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC -- 4-component ETC2, 4x4 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 4,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_R11_EAC, -- 1-component ETC, 4x4 blocks, unsigned normalized
      GL_COMPRESSED_SIGNED_R11_EAC -- 1-component ETC, 4x4 blocks, signed normalized
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 64,
      glFormatSize'blockWidth = 4,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RG11_EAC, -- 2-component ETC, 4x4 blocks, unsigned normalized
      GL_COMPRESSED_SIGNED_RG11_EAC -- 2-component ETC, 4x4 blocks, signed normalized
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 4,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 1
    }

  --
  -- PVRTC
  --

  x | x `elem` [
      GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG, -- 3-component PVRTC, 16x8 blocks, unsigned normalized
      GL_COMPRESSED_SRGB_PVRTC_2BPPV1_EXT, -- 3-component PVRTC, 16x8 blocks, sRGB
      GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG, -- 4-component PVRTC, 16x8 blocks, unsigned normalized
      GL_COMPRESSED_SRGB_ALPHA_PVRTC_2BPPV1_EXT -- 4-component PVRTC, 16x8 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 64,
      glFormatSize'blockWidth = 16,
      glFormatSize'blockHeight = 8,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG, -- 3-component PVRTC, 8x8 blocks, unsigned normalized
      GL_COMPRESSED_SRGB_PVRTC_4BPPV1_EXT, -- 3-component PVRTC, 8x8 blocks, sRGB
      GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG, -- 4-component PVRTC, 8x8 blocks, unsigned normalized
      GL_COMPRESSED_SRGB_ALPHA_PVRTC_4BPPV1_EXT -- 4-component PVRTC, 8x8 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 64,
      glFormatSize'blockWidth = 8,
      glFormatSize'blockHeight = 8,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_PVRTC_2BPPV2_IMG, -- 4-component PVRTC, 8x4 blocks, unsigned normalized
      GL_COMPRESSED_SRGB_ALPHA_PVRTC_2BPPV2_IMG -- 4-component PVRTC, 8x4 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 64,
      glFormatSize'blockWidth = 8,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_PVRTC_4BPPV2_IMG, -- 4-component PVRTC, 4x4 blocks, unsigned normalized
      GL_COMPRESSED_SRGB_ALPHA_PVRTC_4BPPV2_IMG -- 4-component PVRTC, 4x4 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 64,
      glFormatSize'blockWidth = 4,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 1
    }

  --
  -- ASTC
  --

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_4x4_KHR, -- 4-component ASTC, 4x4 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR -- 4-component ASTC, 4x4 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 4,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_5x4_KHR, -- 4-component ASTC, 5x4 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR -- 4-component ASTC, 5x4 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 5,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_5x5_KHR, -- 4-component ASTC, 5x5 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR -- 4-component ASTC, 5x5 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 5,
      glFormatSize'blockHeight = 5,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_6x5_KHR, -- 4-component ASTC, 6x5 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR -- 4-component ASTC, 6x5 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 6,
      glFormatSize'blockHeight = 5,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_6x6_KHR, -- 4-component ASTC, 6x6 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR -- 4-component ASTC, 6x6 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 6,
      glFormatSize'blockHeight = 6,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_8x5_KHR, -- 4-component ASTC, 8x5 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR -- 4-component ASTC, 8x5 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 8,
      glFormatSize'blockHeight = 5,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_8x6_KHR, -- 4-component ASTC, 8x6 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR -- 4-component ASTC, 8x6 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 8,
      glFormatSize'blockHeight = 6,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_8x8_KHR, -- 4-component ASTC, 8x8 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR -- 4-component ASTC, 8x8 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 8,
      glFormatSize'blockHeight = 8,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_10x5_KHR, -- 4-component ASTC, 10x5 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR -- 4-component ASTC, 10x5 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 10,
      glFormatSize'blockHeight = 5,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_10x6_KHR, -- 4-component ASTC, 10x6 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR -- 4-component ASTC, 10x6 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 10,
      glFormatSize'blockHeight = 6,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_10x8_KHR, -- 4-component ASTC, 10x8 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR -- 4-component ASTC, 10x8 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 10,
      glFormatSize'blockHeight = 8,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_10x10_KHR, -- 4-component ASTC, 10x10 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR -- 4-component ASTC, 10x10 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 10,
      glFormatSize'blockHeight = 10,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_12x10_KHR, -- 4-component ASTC, 12x10 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR -- 4-component ASTC, 12x10 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 12,
      glFormatSize'blockHeight = 10,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_12x12_KHR, -- 4-component ASTC, 12x12 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR -- 4-component ASTC, 12x12 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 12,
      glFormatSize'blockHeight = 12,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_3x3x3_OES, -- 4-component ASTC, 3x3x3 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_3x3x3_OES -- 4-component ASTC, 3x3x3 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 3,
      glFormatSize'blockHeight = 3,
      glFormatSize'blockDepth = 3
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_4x3x3_OES, -- 4-component ASTC, 4x3x3 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x3x3_OES -- 4-component ASTC, 4x3x3 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 4,
      glFormatSize'blockHeight = 3,
      glFormatSize'blockDepth = 3
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_4x4x3_OES, -- 4-component ASTC, 4x4x3 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4x3_OES -- 4-component ASTC, 4x4x3 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 4,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 3
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_4x4x4_OES, -- 4-component ASTC, 4x4x4 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4x4_OES -- 4-component ASTC, 4x4x4 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 4,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 4
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_5x4x4_OES, -- 4-component ASTC, 5x4x4 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4x4_OES -- 4-component ASTC, 5x4x4 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 5,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 4
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_5x5x4_OES, -- 4-component ASTC, 5x5x4 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5x4_OES -- 4-component ASTC, 5x5x4 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 5,
      glFormatSize'blockHeight = 5,
      glFormatSize'blockDepth = 4
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_5x5x5_OES, -- 4-component ASTC, 5x5x5 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5x5_OES -- 4-component ASTC, 5x5x5 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 5,
      glFormatSize'blockHeight = 5,
      glFormatSize'blockDepth = 5
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_6x5x5_OES, -- 4-component ASTC, 6x5x5 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5x5_OES -- 4-component ASTC, 6x5x5 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 6,
      glFormatSize'blockHeight = 5,
      glFormatSize'blockDepth = 5
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_6x6x5_OES, -- 4-component ASTC, 6x6x5 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6x5_OES -- 4-component ASTC, 6x6x5 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 6,
      glFormatSize'blockHeight = 6,
      glFormatSize'blockDepth = 5
    }

  x | x `elem` [
      GL_COMPRESSED_RGBA_ASTC_6x6x6_OES, -- 4-component ASTC, 6x6x6 blocks, unsigned normalized
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6x6_OES -- 4-component ASTC, 6x6x6 blocks, sRGB
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 6,
      glFormatSize'blockHeight = 6,
      glFormatSize'blockDepth = 6
    }

  --
  -- ATC
  --

  GL_ATC_RGB_AMD -> -- 3-component, 4x4 blocks, unsigned normalized
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 64,
      glFormatSize'blockWidth = 4,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_ATC_RGBA_EXPLICIT_ALPHA_AMD, -- 4-component, 4x4 blocks, unsigned normalized
      GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD -- 4-component, 4x4 blocks, unsigned normalized
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_COMPRESSED_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 128,
      glFormatSize'blockWidth = 4,
      glFormatSize'blockHeight = 4,
      glFormatSize'blockDepth = 1
    }

  --
  -- Palletized
  --

  GL_PALETTE4_RGB8_OES -> -- 3-component 8,8,8,   4-bit palette, unsigned normalized
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_PALETTIZED_BIT,
      glFormatSize'paletteSizeInBits = 16 * 24,
      glFormatSize'blockSizeInBits = 4,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  GL_PALETTE4_RGBA8_OES -> -- 4-component 8,8,8,8, 4-bit palette, unsigned normalized
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_PALETTIZED_BIT,
      glFormatSize'paletteSizeInBits = 16 * 32,
      glFormatSize'blockSizeInBits = 4,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_PALETTE4_R5_G6_B5_OES, -- 3-component 5,6,5,   4-bit palette, unsigned normalized
      GL_PALETTE4_RGBA4_OES, -- 4-component 4,4,4,4, 4-bit palette, unsigned normalized
      GL_PALETTE4_RGB5_A1_OES -- 4-component 5,5,5,1, 4-bit palette, unsigned normalized
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_PALETTIZED_BIT,
      glFormatSize'paletteSizeInBits = 16 * 16,
      glFormatSize'blockSizeInBits = 4,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  GL_PALETTE8_RGB8_OES -> -- 3-component 8,8,8,   8-bit palette, unsigned normalized
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_PALETTIZED_BIT,
      glFormatSize'paletteSizeInBits = 256 * 24,
      glFormatSize'blockSizeInBits = 8,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  GL_PALETTE8_RGBA8_OES -> -- 4-component 8,8,8,8, 8-bit palette, unsigned normalized
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_PALETTIZED_BIT,
      glFormatSize'paletteSizeInBits = 256 * 32,
      glFormatSize'blockSizeInBits = 8,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_PALETTE8_R5_G6_B5_OES, -- 3-component 5,6,5,   8-bit palette, unsigned normalized
      GL_PALETTE8_RGBA4_OES, -- 4-component 4,4,4,4, 8-bit palette, unsigned normalized
      GL_PALETTE8_RGB5_A1_OES -- 4-component 5,5,5,1, 8-bit palette, unsigned normalized
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_PALETTIZED_BIT,
      glFormatSize'paletteSizeInBits = 256 * 16,
      glFormatSize'blockSizeInBits = 8,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  --
  -- Depth/stencil
  --

  GL_DEPTH_COMPONENT16 ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_DEPTH_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 16,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_DEPTH_COMPONENT24,
      GL_DEPTH_COMPONENT32,
      GL_DEPTH_COMPONENT32F,
      GL_DEPTH_COMPONENT32F_NV
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_DEPTH_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 32,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  GL_STENCIL_INDEX1 ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_STENCIL_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 1,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  GL_STENCIL_INDEX4 ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_STENCIL_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 4,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  GL_STENCIL_INDEX8 ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_STENCIL_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 8,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  GL_STENCIL_INDEX16 ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_STENCIL_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 16,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  GL_DEPTH24_STENCIL8 ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_DEPTH_BIT .|. GL_FORMAT_SIZE_STENCIL_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 32,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  x | x `elem` [
      GL_DEPTH32F_STENCIL8,
      GL_DEPTH32F_STENCIL8_NV
    ] ->
    GlFormatSize {
      glFormatSize'flags = GL_FORMAT_SIZE_DEPTH_BIT .|. GL_FORMAT_SIZE_STENCIL_BIT,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 64,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }

  _ ->
    GlFormatSize {
      glFormatSize'flags = 0,
      glFormatSize'paletteSizeInBits = 0,
      glFormatSize'blockSizeInBits = 8,
      glFormatSize'blockWidth = 1,
      glFormatSize'blockHeight = 1,
      glFormatSize'blockDepth = 1
    }
