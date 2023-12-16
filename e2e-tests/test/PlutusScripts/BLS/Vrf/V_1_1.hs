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

module PlutusScripts.BLS.Vrf.V_1_1 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusScripts.BLS.Common (blsAssetName)
import PlutusScripts.BLS.Vrf.Common (redeemerParams, verifyBlsVrfScript)
import PlutusScripts.Helpers qualified as H
import PlutusTx qualified

verifyBlsVrfPolicyV3 :: SerialisedScript
verifyBlsVrfPolicyV3 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||mkUntypedMintingPolicy @PlutusV3.ScriptContext verifyBlsVrfScript||])

verifyBlsVrfPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifyBlsVrfPolicyScriptV3 = C.PlutusScriptSerialised verifyBlsVrfPolicyV3

verifyBlsVrfAssetIdV3 :: C.AssetId
verifyBlsVrfAssetIdV3 = C.AssetId (H.policyIdV3 verifyBlsVrfPolicyV3) blsAssetName

verifyBlsVrfRedeemer :: C.HashableScriptData
verifyBlsVrfRedeemer = H.toScriptData redeemerParams

verifyBlsVrfMintWitnessV3
  :: C.ShelleyBasedEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsVrfMintWitnessV3 sbe =
  ( H.policyIdV3 verifyBlsVrfPolicyV3
  , H.mintScriptWitness sbe H.plutusL3 (Left verifyBlsVrfPolicyScriptV3) verifyBlsVrfRedeemer
  )
