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

data HashingParams = HashingParams
  { sha2_256Long :: InputOutput
  , sha2_256Short :: InputOutput
  , sha3_256Long :: InputOutput
  , sha3_256Short :: InputOutput
  , blake2b_256Long :: InputOutput
  , blake2b_256Short :: InputOutput
  -- , blake2b_224Long :: InputOutput
  -- , blake2b_224Short :: InputOutput
  -- , keccak256Long :: InputOutput
  -- , keccak256Short :: InputOutput
  }
PlutusTx.unstableMakeIsData ''HashingParams
PlutusTx.makeLift ''HashingParams

-- Use redeemer once PlutusV3 is fully implemented in the ledger
-- {-# INLINEABLE mkHashingPolicy #-}
-- mkHashingPolicy :: HashingParams -> P.BuiltinData -> Bool
-- mkHashingPolicy HashingParams{..} _sc =
--   P.all
--     (P.== True)
--     [ hashAndCheckResult BI.sha2_256 "sha2_256Long" sha2_256Long
--     , hashAndCheckResult BI.sha2_256 "sha2_256Short" sha2_256Short
--     , hashAndCheckResult BI.sha3_256 "sha3_256Long" sha3_256Long
--     , hashAndCheckResult BI.sha3_256 "sha3_256Short" sha3_256Short
--     , hashAndCheckResult BI.blake2b_256 "blake2b_256Long" blake2b_256Long
--     , hashAndCheckResult BI.blake2b_256 "blake2b_256Short" blake2b_256Short
--     ]
--   where
--     hashAndCheckResult
--       :: (P.BuiltinByteString -> P.BuiltinByteString) -> BI.BuiltinString -> InputOutput -> Bool
--     hashAndCheckResult f fName io =
--       P.traceIfFalse ("Hash check failed for : " P.<> fName) (f (input io) P.== output io)

{-# INLINEABLE mkHashingPolicy #-}
mkHashingPolicy :: HashingParams -> P.BuiltinData -> P.BuiltinData -> Bool
mkHashingPolicy HashingParams{..} _r _sc =
  P.all
    (P.== True)
    [ hashAndCheckResult BI.sha2_256 "sha2_256Long" sha2_256Long
    , hashAndCheckResult BI.sha2_256 "sha2_256Short" sha2_256Short
    , hashAndCheckResult BI.sha3_256 "sha3_256Long" sha3_256Long
    , hashAndCheckResult BI.sha3_256 "sha3_256Short" sha3_256Short
    , hashAndCheckResult BI.blake2b_256 "blake2b_256Long" blake2b_256Long
    , hashAndCheckResult BI.blake2b_256 "blake2b_256Short" blake2b_256Short
    ]
  where
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
        BI.toBuiltin $
          bytesFromHex
            ( "54686520626c61636b2076616375756d206f662074686520756e6976657273652"
                <> "e204974207761732064657369676e65642e20546f207377616c6c6f77207573"
                <> "2077686f6c652e20497427732061206c6f73696e672067616d652e"
            )
    , output =
        BI.toBuiltin $
          bytesFromHex "9da361d772347f07fa9e3e05effe677832a694be685e09d6927819052abb7380"
    }

sha2_256ShortIO :: InputOutput
sha2_256ShortIO =
  InputOutput
    { input = BI.toBuiltin $ bytesFromHex "53686f72742074657374206d657373616765"
    , output =
        BI.toBuiltin $
          bytesFromHex "198562f9eebef317e911e4064345b3629f5ba51e554e0d10278f18887850df3a"
    }

sha3_256LongIO :: InputOutput
sha3_256LongIO =
  InputOutput
    { input =
        BI.toBuiltin $
          bytesFromHex
            ( "54686520626c61636b2076616375756d206f662074686520756e6976657273652"
                <> "e204974207761732064657369676e65642e20546f207377616c6c6f77207573"
                <> "2077686f6c652e20497427732061206c6f73696e672067616d652e"
            )
    , output =
        BI.toBuiltin $
          bytesFromHex "222855a086033ff78b8a076245315586cf11443e4f5366203b6d8e05110b9fe8"
    }

sha3_256ShortIO :: InputOutput
sha3_256ShortIO =
  InputOutput
    { input = BI.toBuiltin $ bytesFromHex "53686f72742074657374206d657373616765"
    , output =
        BI.toBuiltin $
          bytesFromHex "04ca8799832111c5079b2811412b7666c71fad8782b74dee4f6d42a08ef7d911"
    }

blake2b_256LongIO :: InputOutput
blake2b_256LongIO =
  InputOutput
    { input =
        BI.toBuiltin $
          bytesFromHex
            ( "54686520626c61636b2076616375756d206f662074686520756e6976657273652e"
                <> "204974207761732064657369676e65642e20546f207377616c6c6f7720757320776"
                <> "86f6c652e20497427732061206c6f73696e672067616d652e"
            )
    , output =
        BI.toBuiltin $
          bytesFromHex "98dc9ddd0021a431d069c5821ba0824442aba3f01138171af8300151ac401155"
    }

blake2b_256ShortIO :: InputOutput
blake2b_256ShortIO =
  InputOutput
    { input = BI.toBuiltin $ bytesFromHex "53686f72742074657374206d657373616765"
    , output =
        BI.toBuiltin $
          bytesFromHex "62250e24b1842531714afbf8bb8d4f5ecc78874f0c6a63c529c3f601c5c77c0e"
    }

hashingParamsV1AndV2 :: HashingParams
hashingParamsV1AndV2 =
  HashingParams
    { sha2_256Long = sha2_256LongIO
    , sha2_256Short = sha2_256ShortIO
    , sha3_256Long = sha3_256LongIO
    , sha3_256Short = sha3_256ShortIO
    , blake2b_256Long = blake2b_256LongIO
    , blake2b_256Short = blake2b_256ShortIO
    }

hashingParamsV3 :: HashingParams
hashingParamsV3 =
  HashingParams
    { sha2_256Long = sha2_256LongIO
    , sha2_256Short = sha2_256ShortIO
    , sha3_256Long = sha3_256LongIO
    , sha3_256Short = sha3_256ShortIO
    , blake2b_256Long = blake2b_256LongIO
    , blake2b_256Short = blake2b_256ShortIO
    -- , blake2b_224Long =  blake2b_224LongIO
    -- , blake2b_224Short = blake2b_224ShortIO
    -- , keccak256Long = keccak256LongIO
    -- , keccak256Short :: keccak256ShortIO
    }

-- Test inputs and outputs for PlutusV1 and PlutusV2 hashing functions
hashingParamsV1AndV2Redeemer :: C.HashableScriptData
hashingParamsV1AndV2Redeemer = toScriptData hashingParamsV1AndV2

-- Test inputs and outputs for PlutusV1, PlutusV2 and PlutusV3 hashing functions
hashingParamsV3Redeemer :: C.HashableScriptData
hashingParamsV3Redeemer = toScriptData hashingParamsV3
