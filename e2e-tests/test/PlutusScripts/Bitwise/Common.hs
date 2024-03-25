{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module PlutusScripts.Bitwise.Common where

import Cardano.Api qualified as C
import PlutusScripts.Helpers (bytesFromHex)
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
  , outputMinSize :: Integer
  , integer :: Integer
  , expByteString :: P.BuiltinByteString
  }
PlutusTx.unstableMakeIsData ''IntegerToByteStringParams

{-# INLINEABLE mkByteStringToIntegerPolicy #-}
mkByteStringToIntegerPolicy :: ByteStringToIntegerParams -> sc -> Bool
mkByteStringToIntegerPolicy ByteStringToIntegerParams{..} _sc = do
  let int = BI.byteStringToInteger bsByteOrder byteString
  int P.== expInteger

{-# INLINEABLE mkIntegerToByteStringPolicy #-}
mkIntegerToByteStringPolicy :: IntegerToByteStringParams -> sc -> Bool
mkIntegerToByteStringPolicy IntegerToByteStringParams{..} _sc = do
  let bs = BI.integerToByteString intByteOrder outputMinSize integer
  bs P.== expByteString

{-# INLINEABLE mkByteStringToIntegerRoundtripPolicy #-}
mkByteStringToIntegerRoundtripPolicy :: P.BuiltinByteString -> sc -> Bool
mkByteStringToIntegerRoundtripPolicy bs _sc = do
  let intBE = BI.byteStringToInteger True bs
      bsBE = BI.integerToByteString True 0 intBE
      intLE = BI.byteStringToInteger False bs
      bsLE = BI.integerToByteString False 0 intLE
  bs P.== bsBE P.&& bs P.== bsLE

bitwiseAssetName :: C.AssetName
bitwiseAssetName = C.AssetName "bitwise"

bsToIParams :: ByteStringToIntegerParams
bsToIParams =
  ByteStringToIntegerParams
    { bsByteOrder = True
    , byteString = P.toBuiltin $ bytesFromHex "deadbeef"
    , expInteger = 3735928559
    }

iToBsParams :: IntegerToByteStringParams
iToBsParams =
  IntegerToByteStringParams
    { intByteOrder = True
    , integer = 3735928559
    , outputMinSize = 0
    , expByteString = P.toBuiltin $ bytesFromHex "deadbeef"
    }
