{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.SECP256k1.V_1_1 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Helpers (
  mintScriptWitness,
  plutusL3,
  policyIdV3,
 )
import PlutusScripts.SECP256k1.Common (
  ecdsaAssetName,
  mkVerifyEcdsaPolicy,
  mkVerifySchnorrPolicy,
  schnorrAssetName,
  verifyEcdsaParams,
  verifyEcdsaRedeemer,
  verifySchnorrParams,
  verifySchnorrRedeemer,
 )
import PlutusTx qualified

-- Schnorr minting policy --

-- verifySchnorrPolicy :: SerialisedScript
-- verifySchnorrPolicy = serialiseCompiledCode $$(PlutusTx.compile [||wrap||])
--   where
--     wrap = mkUntypedMintingPolicy mkVerifySchnorrPolicy

verifySchnorrPolicy :: SerialisedScript
verifySchnorrPolicy =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||mkVerifySchnorrPolicy||])
      `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode plcVersion110 verifySchnorrParams)

verifySchnorrPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifySchnorrPolicyScriptV3 = C.PlutusScriptSerialised verifySchnorrPolicy

verifySchnorrAssetIdV3 :: C.AssetId
verifySchnorrAssetIdV3 = C.AssetId (policyIdV3 verifySchnorrPolicy) schnorrAssetName

verifySchnorrMintWitnessV3
  :: C.ShelleyBasedEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifySchnorrMintWitnessV3 era =
  ( policyIdV3 verifySchnorrPolicy
  , mintScriptWitness era plutusL3 (Left verifySchnorrPolicyScriptV3) verifySchnorrRedeemer
  )

-- ECDSA minting policy --

-- verifyEcdsaPolicy :: SerialisedScript
-- verifyEcdsaPolicy = serialiseCompiledCode $$(PlutusTx.compile [||wrap||])
--   where
--     wrap = mkUntypedMintingPolicy mkVerifyEcdsaPolicy

verifyEcdsaPolicy :: SerialisedScript
verifyEcdsaPolicy =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||mkVerifyEcdsaPolicy||])
      `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode plcVersion110 verifyEcdsaParams)

verifyEcdsaPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifyEcdsaPolicyScriptV3 = C.PlutusScriptSerialised verifyEcdsaPolicy

verifyEcdsaAssetIdV3 :: C.AssetId
verifyEcdsaAssetIdV3 = C.AssetId (policyIdV3 verifyEcdsaPolicy) ecdsaAssetName

verifyEcdsaMintWitnessV3
  :: C.ShelleyBasedEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyEcdsaMintWitnessV3 sbe =
  ( policyIdV3 verifyEcdsaPolicy
  , mintScriptWitness sbe plutusL3 (Left verifyEcdsaPolicyScriptV3) verifyEcdsaRedeemer
  )
