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
import PlutusTx.Prelude qualified as P

data ByteStringToIntegerParams = ByteStringToIntegerParams
  { bsByteOrder :: Bool
  , byteString :: P.BuiltinByteString
  , expInteger :: Integer
  }
$(PlutusTx.unstableMakeIsData ''ByteStringToIntegerParams)

data IntegerToByteStringParams = IntegerToByteStringParams
  { intByteOrder :: Bool
  , outputMinSize :: Integer
  , integer :: Integer
  , expByteString :: P.BuiltinByteString
  }
$(PlutusTx.unstableMakeIsData ''IntegerToByteStringParams)

bitwiseAssetName :: C.AssetName
bitwiseAssetName = C.AssetName "bitwise"

bsToIParams :: ByteStringToIntegerParams
bsToIParams =
  ByteStringToIntegerParams
    { bsByteOrder = True
    , byteString = P.toBuiltin $ bytesFromHex "deadbeef"
    , expInteger = 3_735_928_559
    }

iToBsParams :: IntegerToByteStringParams
iToBsParams =
  IntegerToByteStringParams
    { intByteOrder = True
    , integer = 3_735_928_559
    , outputMinSize = 0
    , expByteString = P.toBuiltin $ bytesFromHex "deadbeef"
    }
