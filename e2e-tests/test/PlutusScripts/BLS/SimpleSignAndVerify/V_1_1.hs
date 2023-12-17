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

module PlutusScripts.BLS.SimpleSignAndVerify.V_1_1 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusScripts.BLS.Common (blsAssetName)
import PlutusScripts.BLS.SimpleSignAndVerify.Common (redeemerParams, verifyBlsSimpleScript)
import PlutusScripts.Helpers qualified as H
import PlutusTx qualified

verifyBlsSimplePolicyV3 :: SerialisedScript
verifyBlsSimplePolicyV3 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||mkUntypedMintingPolicy @PlutusV3.ScriptContext verifyBlsSimpleScript||])

verifyBlsSimplePolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifyBlsSimplePolicyScriptV3 = C.PlutusScriptSerialised verifyBlsSimplePolicyV3

verifyBlsSimpleAssetIdV3 :: C.AssetId
verifyBlsSimpleAssetIdV3 = C.AssetId (H.policyIdV3 verifyBlsSimplePolicyV3) blsAssetName

verifyBlsSimpleRedeemer :: C.HashableScriptData
verifyBlsSimpleRedeemer = H.toScriptData redeemerParams

verifyBlsSimpleMintWitnessV3
  :: C.ShelleyBasedEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsSimpleMintWitnessV3 sbe =
  ( H.policyIdV3 verifyBlsSimplePolicyV3
  , H.mintScriptWitness sbe H.plutusL3 (Left verifyBlsSimplePolicyScriptV3) verifyBlsSimpleRedeemer
  )
