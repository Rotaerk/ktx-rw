{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Codec.Image.Ktx where

import Data.Word

type GLEnum = Word32

-- GL Formats
pattern GL_RED = 0x1903
pattern GL_GREEN = 0x1904
pattern GL_BLUE = 0x1905
pattern GL_ALPHA = 0x1906
pattern GL_LUMINANCE = 0x1909
pattern GL_SLUMINANCE = 0x8C46
pattern GL_LUMINANCE_ALPHA = 0x190A
pattern GL_SLUMINANCE_ALPHA = 0x8C44
pattern GL_INTENSITY = 0x8049
pattern GL_RG = 0x8227
pattern GL_RGB = 0x1907
pattern GL_BGR = 0x80E0
pattern GL_RGBA = 0x1908
pattern GL_BGRA = 0x80E1
pattern GL_RED_INTEGER = 0x8D94
pattern GL_GREEN_INTEGER = 0x8D95
pattern GL_BLUE_INTEGER = 0x8D96
pattern GL_ALPHA_INTEGER = 0x8D97
pattern GL_LUMINANCE_INTEGER = 0x8D9C
pattern GL_LUMINANCE_ALPHA_INTEGER = 0x8D9D
pattern GL_RG_INTEGER = 0x8228
pattern GL_RGB_INTEGER = 0x8D98
pattern GL_BGR_INTEGER = 0x8D9A
pattern GL_RGBA_INTEGER = 0x8D99
pattern GL_BGRA_INTEGER = 0x8D9B
pattern GL_COLOR_INDEX = 0x1900
pattern GL_STENCIL_INDEX = 0x1901
pattern GL_DEPTH_COMPONENT = 0x1902
pattern GL_DEPTH_STENCIL = 0x84F9

-- GL Types
pattern GL_BYTE = 0x1400
pattern GL_UNSIGNED_BYTE = 0x1401
pattern GL_SHORT = 0x1402
pattern GL_UNSIGNED_SHORT = 0x1403
pattern GL_INT = 0x1404
pattern GL_UNSIGNED_INT = 0x1405
pattern GL_INT64 = 0x140E
pattern GL_UNSIGNED_INT64 = 0x140F
pattern GL_HALF_FLOAT = 0x140B
pattern GL_HALF_FLOAT_OES = 0x8D61
pattern GL_FLOAT = 0x1406
pattern GL_DOUBLE = 0x140A
pattern GL_UNSIGNED_BYTE_3_3_2 = 0x8032
pattern GL_UNSIGNED_BYTE_2_3_3_REV = 0x8362
pattern GL_UNSIGNED_SHORT_5_6_5 = 0x8363
pattern GL_UNSIGNED_SHORT_5_6_5_REV = 0x8364
pattern GL_UNSIGNED_SHORT_4_4_4_4 = 0x8033
pattern GL_UNSIGNED_SHORT_4_4_4_4_REV = 0x8365
pattern GL_UNSIGNED_SHORT_5_5_5_1 = 0x8034
pattern GL_UNSIGNED_SHORT_1_5_5_5_REV = 0x8366
pattern GL_UNSIGNED_INT_8_8_8_8 = 0x8035
pattern GL_UNSIGNED_INT_8_8_8_8_REV = 0x8367
pattern GL_UNSIGNED_INT_10_10_10_2 = 0x8036
pattern GL_UNSIGNED_INT_2_10_10_10_REV = 0x8368
pattern GL_UNSIGNED_INT_10F_11F_11F_REV = 0x8C3B
pattern GL_UNSIGNED_INT_5_9_9_9_REV = 0x8C3E
pattern GL_UNSIGNED_INT_24_8 = 0x84FA
pattern GL_FLOAT_32_UNSIGNED_INT_24_8_REV = 0x8DAD