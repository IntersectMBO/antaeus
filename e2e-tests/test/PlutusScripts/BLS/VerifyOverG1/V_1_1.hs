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

module PlutusScripts.BLS.VerifyOverG1.V_1_1 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusCore.Core qualified as PLC
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusScripts.BLS.Common (blsAssetName, blsSigBls12381G2XmdSha256SswuRoNul)
import PlutusScripts.BLS.VerifyOverG1.Common (redeemerParams, verifySigG1Script)
import PlutusScripts.Helpers qualified as H
import PlutusTx qualified

verifyBlsSigG1PolicyV3 :: SerialisedScript
verifyBlsSigG1PolicyV3 =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||\a -> mkUntypedMintingPolicy @PlutusV3.ScriptContext (verifySigG1Script a)||])
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PLC.plcVersion110 blsSigBls12381G2XmdSha256SswuRoNul

verifyBlsSigG1PolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifyBlsSigG1PolicyScriptV3 = C.PlutusScriptSerialised verifyBlsSigG1PolicyV3

verifyBlsSigG1AssetIdV3 :: C.AssetId
verifyBlsSigG1AssetIdV3 = C.AssetId (H.policyIdV3 verifyBlsSigG1PolicyV3) blsAssetName

verifyBlsSigG1Redeemer :: C.HashableScriptData
verifyBlsSigG1Redeemer = H.toScriptData redeemerParams

verifyBlsSigG1MintWitnessV3
  :: C.ShelleyBasedEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsSigG1MintWitnessV3 sbe =
  ( H.policyIdV3 verifyBlsSigG1PolicyV3
  , H.mintScriptWitness sbe H.plutusL3 (Left verifyBlsSigG1PolicyScriptV3) verifyBlsSigG1Redeemer
  )
