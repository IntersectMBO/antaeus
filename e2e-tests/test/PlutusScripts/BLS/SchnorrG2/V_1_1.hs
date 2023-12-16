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

module PlutusScripts.BLS.SchnorrG2.V_1_1 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusCore.Core qualified as PLC
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusScripts.BLS.Common (blsAssetName, byteString16Null)
import PlutusScripts.BLS.SchnorrG2.Common (redeemerParams, verifySchnorrG2Script)
import PlutusScripts.Helpers qualified as H
import PlutusTx qualified

verifyBlsSchnorrG2PolicyV3 :: SerialisedScript
verifyBlsSchnorrG2PolicyV3 =
  serialiseCompiledCode $
    $$( PlutusTx.compile
          [||\a -> mkUntypedMintingPolicy @PlutusV3.ScriptContext (verifySchnorrG2Script a)||]
      )
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PLC.plcVersion110 byteString16Null

verifyBlsSchnorrG2PolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifyBlsSchnorrG2PolicyScriptV3 = C.PlutusScriptSerialised verifyBlsSchnorrG2PolicyV3

verifyBlsSchnorrG2AssetIdV3 :: C.AssetId
verifyBlsSchnorrG2AssetIdV3 = C.AssetId (H.policyIdV3 verifyBlsSchnorrG2PolicyV3) blsAssetName

verifyBlsSchnorrG2Redeemer :: C.HashableScriptData
verifyBlsSchnorrG2Redeemer = H.toScriptData redeemerParams

verifyBlsSchnorrG2MintWitnessV3
  :: C.ShelleyBasedEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsSchnorrG2MintWitnessV3 sbe =
  ( H.policyIdV3 verifyBlsSchnorrG2PolicyV3
  , H.mintScriptWitness
      sbe
      H.plutusL3
      (Left verifyBlsSchnorrG2PolicyScriptV3)
      verifyBlsSchnorrG2Redeemer
  )
