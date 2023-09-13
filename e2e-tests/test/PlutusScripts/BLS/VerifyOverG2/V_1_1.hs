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

module PlutusScripts.BLS.VerifyOverG2.V_1_1 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusCore.Core qualified as PLC
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusScripts.BLS.Common (blsAssetName, blsSigBls12381G2XmdSha256SswuRoNul)
import PlutusScripts.BLS.VerifyOverG2.Common (redeemerParams, verifySigG2Script)
import PlutusScripts.Helpers qualified as H
import PlutusTx qualified

verifyBlsSigG2PolicyV3 :: SerialisedScript
verifyBlsSigG2PolicyV3 =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||\a -> mkUntypedMintingPolicy @PlutusV3.ScriptContext (verifySigG2Script a)||])
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PLC.plcVersion110 blsSigBls12381G2XmdSha256SswuRoNul

verifyBlsSigG2PolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifyBlsSigG2PolicyScriptV3 = C.PlutusScriptSerialised verifyBlsSigG2PolicyV3

verifyBlsSigG2AssetIdV3 :: C.AssetId
verifyBlsSigG2AssetIdV3 = C.AssetId (H.policyIdV3 verifyBlsSigG2PolicyV3) blsAssetName

verifyBlsSigG2Redeemer :: C.HashableScriptData
verifyBlsSigG2Redeemer = H.toScriptData redeemerParams

verifyBlsSigG2MintWitnessV3
  :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsSigG2MintWitnessV3 era =
  ( H.policyIdV3 verifyBlsSigG2PolicyV3
  , H.mintScriptWitness era H.plutusL3 (Left verifyBlsSigG2PolicyScriptV3) verifyBlsSigG2Redeemer
  )
