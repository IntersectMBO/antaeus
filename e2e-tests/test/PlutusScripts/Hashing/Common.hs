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

module PlutusScripts.Hashing.Common where

import Cardano.Api qualified as C
import PlutusScripts.Helpers (
  bytesFromHex,
  toScriptData,
 )
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude qualified as P

data InputOutput = InputOutput
  { input :: P.BuiltinByteString
  , output :: P.BuiltinByteString
  }
PlutusTx.unstableMakeIsData ''InputOutput
PlutusTx.makeLift ''InputOutput

data V1_V2_HashingParams = V1_V2_HashingParams
  { sha2_256Long :: InputOutput
  , sha2_256Short :: InputOutput
  , sha3_256Long :: InputOutput
  , sha3_256Short :: InputOutput
  , blake2b_256Long :: InputOutput
  , blake2b_256Short :: InputOutput
  }
PlutusTx.unstableMakeIsData ''V1_V2_HashingParams
PlutusTx.makeLift ''V1_V2_HashingParams

data V3_HashingParams = V3_HashingParams
  { v2HashingParams :: V1_V2_HashingParams
  , blake2b_224Long :: InputOutput
  , blake2b_224Short :: InputOutput
  , keccak_256Long :: InputOutput
  , keccak_256Short :: InputOutput
  }
PlutusTx.unstableMakeIsData ''V3_HashingParams
PlutusTx.makeLift ''V3_HashingParams

{-# INLINEABLE mkHashingPolicyV1V2 #-}
mkHashingPolicyV1V2 :: V1_V2_HashingParams -> P.BuiltinData -> P.BuiltinData -> Bool
mkHashingPolicyV1V2 V1_V2_HashingParams{..} _r _sc =
  P.all
    (P.== True)
    [ hashAndCheckResult BI.sha2_256 "sha2_256Long" sha2_256Long
    , hashAndCheckResult BI.sha2_256 "sha2_256Short" sha2_256Short
    , hashAndCheckResult BI.sha3_256 "sha3_256Long" sha3_256Long
    , hashAndCheckResult BI.sha3_256 "sha3_256Short" sha3_256Short
    , hashAndCheckResult BI.blake2b_256 "blake2b_256Long" blake2b_256Long
    , hashAndCheckResult BI.blake2b_256 "blake2b_256Short" blake2b_256Short
    ]

{-# INLINEABLE mkHashingPolicyV3 #-}
mkHashingPolicyV3 :: V3_HashingParams -> P.BuiltinData -> P.BuiltinData -> Bool
mkHashingPolicyV3 V3_HashingParams{..} _r _sc =
  P.all
    (P.== True)
    [ hashAndCheckResult BI.sha2_256 "sha2_256Long" (sha2_256Long v2HashingParams)
    , hashAndCheckResult BI.sha2_256 "sha2_256Short" (sha2_256Short v2HashingParams)
    , hashAndCheckResult BI.sha3_256 "sha3_256Long" (sha3_256Long v2HashingParams)
    , hashAndCheckResult BI.sha3_256 "sha3_256Short" (sha3_256Short v2HashingParams)
    , hashAndCheckResult BI.blake2b_256 "blake2b_256Long" (blake2b_256Long v2HashingParams)
    , hashAndCheckResult BI.blake2b_256 "blake2b_256Short" (blake2b_256Long v2HashingParams)
    , hashAndCheckResult BI.blake2b_224 "blake2b_224Long" blake2b_224Long
    , hashAndCheckResult BI.blake2b_224 "blake2b_224Short" blake2b_224Short
    , hashAndCheckResult BI.keccak_256 "keccak_256Long" keccak_256Long
    , hashAndCheckResult BI.keccak_256 "keccak_256Short" keccak_256Short
    ]

{-# INLINEABLE hashAndCheckResult #-}
hashAndCheckResult
  :: (P.BuiltinByteString -> P.BuiltinByteString) -> BI.BuiltinString -> InputOutput -> Bool
hashAndCheckResult f fName io =
  P.traceIfFalse ("Hash check failed for : " P.<> fName) (f (input io) P.== output io)

hashingAssetName :: C.AssetName
hashingAssetName = C.AssetName "hashing"

-- All test following test vectors were produced using https://github.com/RustCrypto/hashes

sha2_256LongIO :: InputOutput
sha2_256LongIO =
  InputOutput
    { input =
        BI.toBuiltin
          $ bytesFromHex
            ( "54686520626c61636b2076616375756d206f662074686520756e6976657273652e"
                <> "204974207761732064657369676e65642e20546f207377616c6c6f7720757320776"
                <> "2077686f6c652e20497427732061206c6f73696e672067616d652e"
            )
    , output =
        BI.toBuiltin
          $ bytesFromHex "9da361d772347f07fa9e3e05effe677832a694be685e09d6927819052abb7380"
    }

sha2_256ShortIO :: InputOutput
sha2_256ShortIO =
  InputOutput
    { input = BI.toBuiltin $ bytesFromHex "53686f72742074657374206d657373616765"
    , output =
        BI.toBuiltin
          $ bytesFromHex "198562f9eebef317e911e4064345b3629f5ba51e554e0d10278f18887850df3a"
    }

sha3_256LongIO :: InputOutput
sha3_256LongIO =
  InputOutput
    { input =
        BI.toBuiltin
          $ bytesFromHex
            ( "54686520626c61636b2076616375756d206f662074686520756e6976657273652e"
                <> "204974207761732064657369676e65642e20546f207377616c6c6f7720757320776"
                <> "2077686f6c652e20497427732061206c6f73696e672067616d652e"
            )
    , output =
        BI.toBuiltin
          $ bytesFromHex "222855a086033ff78b8a076245315586cf11443e4f5366203b6d8e05110b9fe8"
    }

sha3_256ShortIO :: InputOutput
sha3_256ShortIO =
  InputOutput
    { input = BI.toBuiltin $ bytesFromHex "53686f72742074657374206d657373616765"
    , output =
        BI.toBuiltin
          $ bytesFromHex "04ca8799832111c5079b2811412b7666c71fad8782b74dee4f6d42a08ef7d911"
    }

blake2b_256LongIO :: InputOutput
blake2b_256LongIO =
  InputOutput
    { input =
        BI.toBuiltin
          $ bytesFromHex
            ( "54686520626c61636b2076616375756d206f662074686520756e6976657273652e"
                <> "204974207761732064657369676e65642e20546f207377616c6c6f7720757320776"
                <> "2077686f6c652e20497427732061206c6f73696e672067616d652e"
            )
    , output =
        BI.toBuiltin
          $ bytesFromHex "98dc9ddd0021a431d069c5821ba0824442aba3f01138171af8300151ac401155"
    }

blake2b_256ShortIO :: InputOutput
blake2b_256ShortIO =
  InputOutput
    { input = BI.toBuiltin $ bytesFromHex "53686f72742074657374206d657373616765"
    , output =
        BI.toBuiltin
          $ bytesFromHex "62250e24b1842531714afbf8bb8d4f5ecc78874f0c6a63c529c3f601c5c77c0e"
    }

blake2b_224LongIO :: InputOutput
blake2b_224LongIO =
  InputOutput
    { input =
        BI.toBuiltin
          $ bytesFromHex
            ( "54686520626c61636b2076616375756d206f662074686520756e6976657273652e20"
                <> "4974207761732064657369676e65642e20546f207377616c6c6f77207573207768"
                <> "6f6c652e20497427732061206c6f73696e672067616d652e"
            )
    , output =
        BI.toBuiltin
          $ bytesFromHex "8ad5493e31e61bace07bf6bfdf7931c5f381b1cd80dc093605ce5c2a"
    }

blake2b_224ShortIO :: InputOutput
blake2b_224ShortIO =
  InputOutput
    { input = BI.toBuiltin $ bytesFromHex "53686f72742074657374206d657373616765"
    , output =
        BI.toBuiltin
          $ bytesFromHex "dbf1eb305849b048232dd0a92d4885c36a1d076dbf652a6cd5459244"
    }

keccak256LongIO :: InputOutput
keccak256LongIO =
  InputOutput
    { input =
        BI.toBuiltin
          $ bytesFromHex
            ( "54686520626c61636b2076616375756d206f662074686520756e6976657273652e204974"
                <> "207761732064657369676e65642e20546f207377616c6c6f772075732077686f6c652e20"
                <> "497427732061206c6f73696e672067616d652e"
            )
    , output =
        BI.toBuiltin
          $ bytesFromHex "4a02afccd1aa14dcab8c6afcd9654ef3cde6e634575aac26bf4e4dd5030a50d0"
    }

keccak256ShortIO :: InputOutput
keccak256ShortIO =
  InputOutput
    { input = BI.toBuiltin $ bytesFromHex "53686f72742074657374206d657373616765"
    , output =
        BI.toBuiltin
          $ bytesFromHex "352c3f362f64da546ec7482811f67d1a201f71cad2dad21dc701277f09ff605f"
    }

hashingParamsV1V2 :: V1_V2_HashingParams
hashingParamsV1V2 =
  V1_V2_HashingParams
    { sha2_256Long = sha2_256LongIO
    , sha2_256Short = sha2_256ShortIO
    , sha3_256Long = sha3_256LongIO
    , sha3_256Short = sha3_256ShortIO
    , blake2b_256Long = blake2b_256LongIO
    , blake2b_256Short = blake2b_256ShortIO
    }

hashingParamsV3 :: V3_HashingParams
hashingParamsV3 =
  V3_HashingParams
    { v2HashingParams = hashingParamsV1V2
    , blake2b_224Long = blake2b_224LongIO
    , blake2b_224Short = blake2b_224ShortIO
    , keccak_256Long = keccak256LongIO
    , keccak_256Short = keccak256ShortIO
    }

-- Test inputs and outputs for PlutusV1 and PlutusV2 hashing functions
hashingParamsV1AndV2Redeemer :: C.HashableScriptData
hashingParamsV1AndV2Redeemer = toScriptData hashingParamsV1V2

-- Test inputs and outputs for PlutusV1, PlutusV2 and PlutusV3 hashing functions
hashingParamsV3Redeemer :: C.HashableScriptData
hashingParamsV3Redeemer = toScriptData hashingParamsV3
