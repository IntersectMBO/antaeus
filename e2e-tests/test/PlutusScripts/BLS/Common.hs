{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# HLINT ignore "Use underscore" #-}

module PlutusScripts.BLS.Common where

import Cardano.Api qualified as C
import Data.ByteString.Char8 qualified as C8
import PlutusScripts.Helpers qualified as H
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude qualified as P

blsAssetName :: C.AssetName
blsAssetName = C.AssetName "BLS"

-- hex value 424c535f5349475f424c53313233383147325f584d443a5348412d3235365f535357555f524f5f4e554c5f"
blsSigBls12381G2XmdSha256SswuRoNul :: P.BuiltinByteString
blsSigBls12381G2XmdSha256SswuRoNul = BI.toBuiltin $ C8.pack "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_"

-- all zero bytestring 16 byte in length used to append to truncated bytestring onchain
{-# INLINEABLE byteString16Null #-}
byteString16Null :: P.BuiltinByteString
byteString16Null = P.toBuiltin $ H.bytesFromHex "00000000000000000000000000000000"

-- Little-endian bytestring to integer conversion #-}
{-# INLINEABLE byteStringToIntegerLE #-}
byteStringToIntegerLE :: P.BuiltinByteString -> Integer
byteStringToIntegerLE = BI.byteStringToInteger False

-- original PlutusTx implementation used before CIP-0087 support with https://github.com/IntersectMBO/plutus/pull/5654
-- {-# INLINEABLE byteStringToInteger #-}
-- byteStringToInteger :: P.BuiltinByteString -> Integer
-- byteStringToInteger b =
--   go 0
--   where
--     len = P.lengthOfByteString b
--     go i =
--       if i P.>= len
--         then 0
--         else (P.indexByteString b i) P.+ 256 P.* (go (i P.+ 1))
