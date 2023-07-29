{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.SECP256k1.V_1_0 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusScripts.Helpers (
  mintScriptWitness,
  plutusL1,
  plutusL2,
  policyIdV1,
  policyIdV2,
 )
import PlutusScripts.SECP256k1.Common (
  ecdsaAssetName,
  mkVerifyEcdsaPolicy,
  mkVerifySchnorrPolicy,
  schnorrAssetName,
  verifyEcdsaRedeemer,
  verifySchnorrRedeemer,
 )
import PlutusTx qualified

-- Schnorr minting policy --

verifySchnorrPolicyV1 :: SerialisedScript
verifySchnorrPolicyV1 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV1.ScriptContext mkVerifySchnorrPolicy

verifySchnorrPolicyV2 :: SerialisedScript
verifySchnorrPolicyV2 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV2.ScriptContext mkVerifySchnorrPolicy

verifySchnorrPolicyScriptV1 :: C.PlutusScript C.PlutusScriptV1
verifySchnorrPolicyScriptV1 = C.PlutusScriptSerialised verifySchnorrPolicyV1

verifySchnorrPolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
verifySchnorrPolicyScriptV2 = C.PlutusScriptSerialised verifySchnorrPolicyV2

verifySchnorrAssetIdV1 :: C.AssetId
verifySchnorrAssetIdV1 = C.AssetId (policyIdV1 verifySchnorrPolicyV1) schnorrAssetName

verifySchnorrAssetIdV2 :: C.AssetId
verifySchnorrAssetIdV2 = C.AssetId (policyIdV2 verifySchnorrPolicyV2) schnorrAssetName

verifySchnorrMintWitnessV1
  :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifySchnorrMintWitnessV1 era =
  ( policyIdV1 verifySchnorrPolicyV1
  , mintScriptWitness era plutusL1 (Left verifySchnorrPolicyScriptV1) verifySchnorrRedeemer
  )

verifySchnorrMintWitnessV2
  :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifySchnorrMintWitnessV2 era =
  ( policyIdV2 verifySchnorrPolicyV2
  , mintScriptWitness era plutusL2 (Left verifySchnorrPolicyScriptV2) verifySchnorrRedeemer
  )

-- ECDSA minting policy --

verifyEcdsaPolicyV1 :: SerialisedScript
verifyEcdsaPolicyV1 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV1.ScriptContext mkVerifyEcdsaPolicy

verifyEcdsaPolicyV2 :: SerialisedScript
verifyEcdsaPolicyV2 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV2.ScriptContext mkVerifyEcdsaPolicy

verifyEcdsaPolicyScriptV1 :: C.PlutusScript C.PlutusScriptV1
verifyEcdsaPolicyScriptV1 = C.PlutusScriptSerialised verifyEcdsaPolicyV1

verifyEcdsaPolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
verifyEcdsaPolicyScriptV2 = C.PlutusScriptSerialised verifyEcdsaPolicyV2

verifyEcdsaAssetIdV1 :: C.AssetId
verifyEcdsaAssetIdV1 = C.AssetId (policyIdV1 verifyEcdsaPolicyV1) ecdsaAssetName

verifyEcdsaAssetIdV2 :: C.AssetId
verifyEcdsaAssetIdV2 = C.AssetId (policyIdV2 verifyEcdsaPolicyV2) ecdsaAssetName

verifyEcdsaMintWitnessV1
  :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyEcdsaMintWitnessV1 era =
  ( policyIdV1 verifyEcdsaPolicyV1
  , mintScriptWitness era plutusL1 (Left verifyEcdsaPolicyScriptV1) verifyEcdsaRedeemer
  )

verifyEcdsaMintWitnessV2
  :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyEcdsaMintWitnessV2 era =
  ( policyIdV2 verifyEcdsaPolicyV2
  , mintScriptWitness era plutusL2 (Left verifyEcdsaPolicyScriptV2) verifyEcdsaRedeemer
  )
