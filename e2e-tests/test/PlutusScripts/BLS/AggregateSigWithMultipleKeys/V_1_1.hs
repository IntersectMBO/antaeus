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

module PlutusScripts.BLS.AggregateSigWithMultipleKeys.V_1_1 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusCore.Core qualified as PLC
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusScripts.BLS.AggregateSigWithMultipleKeys.Common (
  aggregateMultiKeyG2Script,
  redeemerParams,
 )
import PlutusScripts.BLS.Common (blsAssetName, blsSigBls12381G2XmdSha256SswuRoNul, byteString16Null)
import PlutusScripts.Helpers qualified as H
import PlutusTx qualified

verifyBlsAggregateSigMultiKeyG2PolicyV3 :: SerialisedScript
verifyBlsAggregateSigMultiKeyG2PolicyV3 =
  serialiseCompiledCode $
    $$( PlutusTx.compile
          [||\a b -> mkUntypedMintingPolicy @PlutusV3.ScriptContext (aggregateMultiKeyG2Script a b)||]
      )
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PLC.plcVersion110 byteString16Null
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PLC.plcVersion110 blsSigBls12381G2XmdSha256SswuRoNul

verifyBlsAggregateSigMultiKeyG2PolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifyBlsAggregateSigMultiKeyG2PolicyScriptV3 = C.PlutusScriptSerialised verifyBlsAggregateSigMultiKeyG2PolicyV3

verifyBlsAggregateSigMultiKeyG2AssetIdV3 :: C.AssetId
verifyBlsAggregateSigMultiKeyG2AssetIdV3 = C.AssetId (H.policyIdV3 verifyBlsAggregateSigMultiKeyG2PolicyV3) blsAssetName

verifyBlsAggregateSigMultiKeyG2Redeemer :: C.HashableScriptData
verifyBlsAggregateSigMultiKeyG2Redeemer = H.toScriptData redeemerParams

verifyBlsAggregateSigMultiKeyG2MintWitnessV3
  :: C.ShelleyBasedEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsAggregateSigMultiKeyG2MintWitnessV3 sbe =
  ( H.policyIdV3 verifyBlsAggregateSigMultiKeyG2PolicyV3
  , H.mintScriptWitness
      sbe
      H.plutusL3
      (Left verifyBlsAggregateSigMultiKeyG2PolicyScriptV3)
      verifyBlsAggregateSigMultiKeyG2Redeemer
  )
