{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Properties where

import Test.QuickCheck

-- Warning: QuickCheck magic ahead
return []
runTests :: IO Bool
runTests = $quickCheckAll
