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

module PlutusScripts.BLS.AggregateSigWithSingleKey.V_1_1 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusCore.Core qualified as PLC
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusScripts.BLS.AggregateSigWithSingleKey.Common (aggregateSigSingleKeyG1, redeemerParams)
import PlutusScripts.BLS.Common (blsAssetName, blsSigBls12381G2XmdSha256SswuRoNul)
import PlutusScripts.Helpers qualified as H
import PlutusTx qualified

verifyAggregateSigSingleKeyG1PolicyV3 :: SerialisedScript
verifyAggregateSigSingleKeyG1PolicyV3 =
  serialiseCompiledCode $
    $$( PlutusTx.compile
          [||\a -> mkUntypedMintingPolicy @PlutusV3.ScriptContext (aggregateSigSingleKeyG1 a)||]
      )
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PLC.plcVersion110 blsSigBls12381G2XmdSha256SswuRoNul

verifyAggregateSigSingleKeyG1PolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifyAggregateSigSingleKeyG1PolicyScriptV3 = C.PlutusScriptSerialised verifyAggregateSigSingleKeyG1PolicyV3

verifyBlsAggregateSigSingleKeyG1AssetIdV3 :: C.AssetId
verifyBlsAggregateSigSingleKeyG1AssetIdV3 = C.AssetId (H.policyIdV3 verifyAggregateSigSingleKeyG1PolicyV3) blsAssetName

verifyAggregateSigSingleKeyG1Redeemer :: C.HashableScriptData
verifyAggregateSigSingleKeyG1Redeemer = H.toScriptData redeemerParams

verifyBlsAggregateSigSingleKeyG1MintWitnessV3
  :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsAggregateSigSingleKeyG1MintWitnessV3 era =
  ( H.policyIdV3 verifyAggregateSigSingleKeyG1PolicyV3
  , H.mintScriptWitness
      era
      H.plutusL3
      (Left verifyAggregateSigSingleKeyG1PolicyScriptV3)
      verifyAggregateSigSingleKeyG1Redeemer
  )
