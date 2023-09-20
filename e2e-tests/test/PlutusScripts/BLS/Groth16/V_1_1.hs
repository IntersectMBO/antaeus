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

module PlutusScripts.BLS.Groth16.V_1_1 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusCore.Core qualified as PLC
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusScripts.BLS.Common (blsAssetName)
import PlutusScripts.BLS.Groth16.Common (
  CompressedG1Element (compressedG1),
  CompressedG2Element (compressedG2),
  groth16Scalar,
  groth16a,
  groth16alpha,
  groth16b,
  groth16beta,
  groth16c,
  groth16delta,
  groth16gamma,
  groth16gamma_abc_1,
  groth16gamma_abc_2,
  verifyBlsGroth16Script,
 )
import PlutusScripts.Helpers qualified as H
import PlutusTx qualified

{- | Make a UPLC script applying groth16Verify to the inputs.  Passing the
 newtype inputs increases the size and CPU cost slightly, so we unwrap them
 first.  This should return `True`.
-}
verifyBlsGroth16PolicyV3 :: SerialisedScript
verifyBlsGroth16PolicyV3 =
  serialiseCompiledCode $
    $$( PlutusTx.compile
          [||
          \a b c d e f g h i ->
            mkUntypedMintingPolicy @PlutusV3.ScriptContext (verifyBlsGroth16Script a b c d e f g h i)
          ||]
      )
      `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode PLC.plcVersion110 $ compressedG1 groth16alpha)
      `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode PLC.plcVersion110 $ compressedG2 groth16beta)
      `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode PLC.plcVersion110 $ compressedG2 groth16gamma)
      `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode PLC.plcVersion110 $ compressedG2 groth16delta)
      `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode PLC.plcVersion110 $ compressedG1 groth16gamma_abc_1)
      `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode PLC.plcVersion110 $ compressedG1 groth16gamma_abc_2)
      `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode PLC.plcVersion110 $ compressedG1 groth16a)
      `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode PLC.plcVersion110 $ compressedG2 groth16b)
      `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode PLC.plcVersion110 $ compressedG1 groth16c)

verifyBlsGroth16PolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifyBlsGroth16PolicyScriptV3 = C.PlutusScriptSerialised verifyBlsGroth16PolicyV3

verifyBlsGroth16AssetIdV3 :: C.AssetId
verifyBlsGroth16AssetIdV3 = C.AssetId (H.policyIdV3 verifyBlsGroth16PolicyV3) blsAssetName

verifyBlsGroth16Redeemer :: C.HashableScriptData
verifyBlsGroth16Redeemer = H.toScriptData groth16Scalar

verifyBlsGroth16MintWitnessV3
  :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyBlsGroth16MintWitnessV3 era =
  ( H.policyIdV3 verifyBlsGroth16PolicyV3
  , H.mintScriptWitness era H.plutusL3 (Left verifyBlsGroth16PolicyScriptV3) verifyBlsGroth16Redeemer
  )
