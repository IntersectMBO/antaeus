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

module PlutusScripts.BLS.SchnorrG1.V_1_1 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import PlutusCore.Core qualified as PLC
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.BLS.Common (blsAssetName, byteString16Null)
import PlutusScripts.BLS.SchnorrG1.Common (redeemerParams, verifySchnorrG1Script)
import PlutusScripts.Helpers qualified as H
import PlutusTx qualified

verifyBlsSchnorrG1PolicyV3 :: SerialisedScript
verifyBlsSchnorrG1PolicyV3 =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||\a -> verifySchnorrG1Script a||])
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PLC.plcVersion110 byteString16Null

verifyBlsSchnorrG1PolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifyBlsSchnorrG1PolicyScriptV3 = C.PlutusScriptSerialised verifyBlsSchnorrG1PolicyV3

verifyBlsSchnorrG1AssetIdV3 :: C.AssetId
verifyBlsSchnorrG1AssetIdV3 = C.AssetId (H.policyIdV3 verifyBlsSchnorrG1PolicyV3) blsAssetName

verifyBlsSchnorrG1Redeemer :: C.HashableScriptData
verifyBlsSchnorrG1Redeemer = H.toScriptData redeemerParams

verifyBlsSchnorrG1MintWitnessV3
  :: C.ShelleyBasedEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsSchnorrG1MintWitnessV3 sbe =
  ( H.policyIdV3 verifyBlsSchnorrG1PolicyV3
  , H.mintScriptWitness
      sbe
      H.plutusL3
      (Left verifyBlsSchnorrG1PolicyScriptV3)
      verifyBlsSchnorrG1Redeemer
  )
