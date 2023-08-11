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

module PlutusScripts.Hashing.V_1_1 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusScripts.Hashing.Common (hashingAssetName, hashingParamsV3Redeemer, mkHashingPolicy)
import PlutusScripts.Helpers (
  mintScriptWitness,
  plutusL3,
  policyIdV3,
 )
import PlutusTx qualified

checkHashingPolicyV3 :: SerialisedScript
checkHashingPolicyV3 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV3.ScriptContext mkHashingPolicy

checkHashingPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
checkHashingPolicyScriptV3 = C.PlutusScriptSerialised checkHashingPolicyV3

checkHashingAssetIdV3 :: C.AssetId
checkHashingAssetIdV3 = C.AssetId (policyIdV3 checkHashingPolicyV3) hashingAssetName

checkHashingMintWitnessV3
  :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
checkHashingMintWitnessV3 era =
  ( policyIdV3 checkHashingPolicyV3
  , mintScriptWitness era plutusL3 (Left checkHashingPolicyScriptV3) hashingParamsV3Redeemer
  )
