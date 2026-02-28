{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.SOP.V_1_1 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Helpers (
  mintScriptWitness,
  plutusL3,
  policyIdV3,
  toScriptData,
 )
import PlutusScripts.SOP.Common (mkSopPolicyV3, sopAssetName)
import PlutusTx qualified

checkSopPolicy :: SerialisedScript
checkSopPolicy =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||mkSopPolicyV3||])

checkSopPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
checkSopPolicyScriptV3 = C.PlutusScriptSerialised checkSopPolicy

checkSopAssetIdV3 :: C.AssetId
checkSopAssetIdV3 = C.AssetId (policyIdV3 checkSopPolicy) sopAssetName

checkSopMintWitnessV3
  :: C.ShelleyBasedEra era
  -> C.HashableScriptData
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
checkSopMintWitnessV3 sbe redeemer =
  ( policyIdV3 checkSopPolicy
  , mintScriptWitness sbe plutusL3 (Left checkSopPolicyScriptV3) redeemer
  )
