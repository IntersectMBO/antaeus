{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.Hashing.V_1_0 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusScripts.Hashing.Common (
  hashingAssetName,
  hashingParamsV1AndV2Redeemer,
  mkHashingPolicy,
 )
import PlutusScripts.Helpers (
  mintScriptWitness,
  plutusL1,
  plutusL2,
  policyIdV1,
  policyIdV2,
 )
import PlutusTx qualified

checkHashingPolicyV1 :: SerialisedScript
checkHashingPolicyV1 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV1.ScriptContext mkHashingPolicy

checkHashingPolicyV2 :: SerialisedScript
checkHashingPolicyV2 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV2.ScriptContext mkHashingPolicy

checkHashingPolicyScriptV1 :: C.PlutusScript C.PlutusScriptV1
checkHashingPolicyScriptV1 = C.PlutusScriptSerialised checkHashingPolicyV1

checkHashingPolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
checkHashingPolicyScriptV2 = C.PlutusScriptSerialised checkHashingPolicyV2

checkHashingAssetIdV1 :: C.AssetId
checkHashingAssetIdV1 = C.AssetId (policyIdV1 checkHashingPolicyV1) hashingAssetName

checkHashingAssetIdV2 :: C.AssetId
checkHashingAssetIdV2 = C.AssetId (policyIdV2 checkHashingPolicyV2) hashingAssetName

checkHashingMintWitnessV1
  :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
checkHashingMintWitnessV1 era =
  ( policyIdV1 checkHashingPolicyV1
  , mintScriptWitness era plutusL1 (Left checkHashingPolicyScriptV1) hashingParamsV1AndV2Redeemer
  )

checkHashingMintWitnessV2
  :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
checkHashingMintWitnessV2 era =
  ( policyIdV2 checkHashingPolicyV2
  , mintScriptWitness era plutusL2 (Left checkHashingPolicyScriptV2) hashingParamsV1AndV2Redeemer
  )
