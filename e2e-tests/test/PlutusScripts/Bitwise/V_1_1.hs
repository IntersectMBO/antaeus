{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Bitwise.V_1_1 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Helpers.ScriptUtils (mkUntypedMintingPolicy)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusScripts.Bitwise.Common (
  ByteStringToIntegerParams,
  IntegerToByteStringParams,
  mkByteStringToIntegerPolicy,
  mkIntegerToByteStringPolicy,
 )
import PlutusScripts.Helpers (
  mintScriptWitness,
  plutusL3,
  policyIdV3,
  toScriptData,
 )
import PlutusTx qualified

-- ByteString to Integer --

byteStringToIntegerPolicyV3 :: SerialisedScript
byteStringToIntegerPolicyV3 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV3.ScriptContext mkByteStringToIntegerPolicy

byteStringToIntegerPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
byteStringToIntegerPolicyScriptV3 = C.PlutusScriptSerialised byteStringToIntegerPolicyV3

byteStringToIntegerAssetIdV3 :: C.AssetId
byteStringToIntegerAssetIdV3 = C.AssetId (policyIdV3 byteStringToIntegerPolicyV3) bitwiseAssetName

byteStringToIntegerMintWitnessV3
  :: C.ShelleyBasedEra era
  -> ByteStringToIntegerParams
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
byteStringToIntegerMintWitnessV3 sbe redeemer =
  ( policyIdV3 byteStringToIntegerPolicyV3
  , mintScriptWitness sbe plutusL3 (Left byteStringToIntegerPolicyScriptV3) (toScriptData redeemer)
  )

-- Integer to ByteString --

integerToByteStringPolicyV3 :: SerialisedScript
integerToByteStringPolicyV3 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV3.ScriptContext mkByteStringToIntegerPolicy

integerToByteStringPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
integerToByteStringPolicyScriptV3 = C.PlutusScriptSerialised integerToByteStringPolicyV3

integerToByteStringAssetIdV3 :: C.AssetId
integerToByteStringAssetIdV3 = C.AssetId (policyIdV3 integerToByteStringPolicyV3) bitwiseAssetName

integerToByteStringMintWitnessV3
  :: C.ShelleyBasedEra era
  -> IntegerToByteStringParams
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
integerToByteStringMintWitnessV3 sbe redeemer =
  ( policyIdV3 integerToByteStringPolicyV3
  , mintScriptWitness sbe plutusL3 (Left integerToByteStringPolicyScriptV3) (toScriptData redeemer)
  )

-- ByteString to Integer and back --

byteStringToIntegerAndBackPolicyV3 :: SerialisedScript
byteStringToIntegerAndBackPolicyV3 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV3.ScriptContext mkByteStringToIntegerAndBackPolicy

byteStringToIntegerAndBackPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
byteStringToIntegerAndBackPolicyScriptV3 = C.PlutusScriptSerialised integerToByteStringAndBackPolicyV3

byteStringToIntegerAndBackAssetIdV3 :: C.AssetId
byteStringToIntegerAndBackAssetIdV3 = C.AssetId (policyIdV3 byteStringToIntegerAndBackPolicyScriptV3) bitwiseAssetName

byteStringToIntegerAndBackMintWitnessV3
  :: C.ShelleyBasedEra era
  -> P.BuiltinByteString
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
byteStringToIntegerAndBackMintWitnessV3 sbe redeemer =
  ( policyIdV3 byteStringToIntegerAndBackPolicyV3
  , mintScriptWitness
      sbe
      plutusL3
      (Left byteStringToIntegerAndBackPolicyScriptV3)
      (toScriptData redeemer)
  )
