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
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Hashing.Common (
  hashingAssetName,
  hashingParamsV1AndV2,
  mkHashingPolicy,
 )
import PlutusScripts.Helpers (
  mintScriptWitness,
  plutusL1,
  plutusL2,
  policyIdV1,
  policyIdV2,
  toScriptData,
 )
import PlutusTx qualified

-- checkHashingPolicyV1 :: SerialisedScript
-- checkHashingPolicyV1 =
--   serialiseCompiledCode
--     $$(PlutusTx.compile [||wrap||])
--   where
--     wrap = mkUntypedMintingPolicy @PlutusV1.ScriptContext mkHashingPolicy

-- checkHashingPolicyV2 :: SerialisedScript
-- checkHashingPolicyV2 =
--   serialiseCompiledCode
--     $$(PlutusTx.compile [||wrap||])
--   where
--     wrap = mkUntypedMintingPolicy @PlutusV2.ScriptContext mkHashingPolicy

checkHashingPolicy :: SerialisedScript
checkHashingPolicy =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||mkHashingPolicy||])
      `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode plcVersion100 hashingParamsV1AndV2)

checkHashingPolicyScriptV1 :: C.PlutusScript C.PlutusScriptV1
checkHashingPolicyScriptV1 = C.PlutusScriptSerialised checkHashingPolicy

checkHashingPolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
checkHashingPolicyScriptV2 = C.PlutusScriptSerialised checkHashingPolicy

checkHashingAssetIdV1 :: C.AssetId
checkHashingAssetIdV1 = C.AssetId (policyIdV1 checkHashingPolicy) hashingAssetName

checkHashingAssetIdV2 :: C.AssetId
checkHashingAssetIdV2 = C.AssetId (policyIdV2 checkHashingPolicy) hashingAssetName

checkHashingMintWitnessV1
  :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
checkHashingMintWitnessV1 era =
  ( policyIdV1 checkHashingPolicy
  , mintScriptWitness era plutusL1 (Left checkHashingPolicyScriptV1) (toScriptData ())
  )

checkHashingMintWitnessV2
  :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
checkHashingMintWitnessV2 era =
  ( policyIdV2 checkHashingPolicy
  , mintScriptWitness era plutusL2 (Left checkHashingPolicyScriptV2) (toScriptData ())
  )
