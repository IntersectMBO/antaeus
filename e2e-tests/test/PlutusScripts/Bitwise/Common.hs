{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module PlutusScripts.Bitwise.Common where

import Cardano.Api qualified as C
import Data.Aeson.Decoding.ByteString (bsToTokens)
import Data.Aeson.Encoding qualified as P
import PlutusScripts.BLS.Common qualified as P
import PlutusScripts.Helpers (
  bytesFromHex,
  toScriptData,
 )
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude qualified as P

data ByteStringToIntegerParams = ByteStringToIntegerParams
  { bsByteOrder :: Bool
  , byteString :: P.BuiltinByteString
  , expInteger :: Integer
  }
PlutusTx.unstableMakeIsData ''ByteStringToIntegerParams

data IntegerToByteStringParams = IntegerToByteStringParams
  { intByteOrder :: Bool
  , integer :: Integer
  , outputSize :: Integer
  , expByteString :: P.BuiltinByteString
  }
PlutusTx.unstableMakeIsData ''IntegerToByteStringParams

{-# INLINEABLE mkByteStringToIntegerPolicy #-}
mkByteStringToIntegerPolicy :: ByteStringToIntegerParams -> P.BuiltinData -> Bool
mkByteStringToIntegerPolicy ByteStringToIntegerParams{..} _sc = do
  let int = P.byteStringToInteger bsByteOrder byteString
  int P.== expInteger

{-# INLINEABLE mkIntegerToByteStringPolicy #-}
mkIntegerToByteStringPolicy :: ByteStringToIntegerParams -> P.BuiltinData -> Bool
mkIntegerToByteStringPolicy ByteStringToIntegerParams{..} _sc = do
  let int = P.integerToByteString bsByteOrder outputSize byteString
  int P.== expInteger

{-# INLINEABLE mkByteStringToIntegerAndBackPolicy #-}
mkByteStringToIntegerAndBackPolicy :: P.BuiltinByteString -> P.BuiltinData -> Bool
mkByteStringToIntegerAndBackPolicy bs _sc = do
  let intBE = P.byteStringToInteger True bs
      bsBE = P.integerToByteString True 0 int
      intLE = P.byteStringToInteger False bs
      bsLE = P.integerToByteString False 0 int
  bs P.== bsBE P.&& bs P.== bsLE

bitwiseAssetName :: C.AssetName
bitwiseAssetName = C.AssetName "bitwise"

-- byteStringToIntegerRedeemer :: P.BuiltinByteString
-- byteStringToIntegerRedeemer = toScriptData "deadbeef"

-- invalidByteStringToIntegerRedeemer :: C.BuiltinByteString
-- invalidByteStringToIntegerRedeemer = toScriptData "abc"

-- integerToByteStringRedeemer :: Integer
-- integerToByteStringRedeemer = toScriptData 2147483648
