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

module PlutusScripts.SECP256k1.V_1_1 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as PlutusV3
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
  verifyEcdsaRedeemer,
  verifySchnorrRedeemer,
 )
import PlutusTx qualified

-- Schnorr minting policy --

verifySchnorrPolicyV3 :: SerialisedScript
verifySchnorrPolicyV3 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV3.ScriptContext mkVerifySchnorrPolicy

verifySchnorrPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifySchnorrPolicyScriptV3 = C.PlutusScriptSerialised verifySchnorrPolicyV3

verifySchnorrAssetIdV3 :: C.AssetId
verifySchnorrAssetIdV3 = C.AssetId (policyIdV3 verifySchnorrPolicyV3) schnorrAssetName

verifySchnorrMintWitnessV3
  :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifySchnorrMintWitnessV3 era =
  ( policyIdV3 verifySchnorrPolicyV3
  , mintScriptWitness era plutusL3 (Left verifySchnorrPolicyScriptV3) verifySchnorrRedeemer
  )

-- ECDSA minting policy --

verifyEcdsaPolicyV3 :: SerialisedScript
verifyEcdsaPolicyV3 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV3.ScriptContext mkVerifyEcdsaPolicy

verifyEcdsaPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifyEcdsaPolicyScriptV3 = C.PlutusScriptSerialised verifyEcdsaPolicyV3

verifyEcdsaAssetIdV3 :: C.AssetId
verifyEcdsaAssetIdV3 = C.AssetId (policyIdV3 verifyEcdsaPolicyV3) ecdsaAssetName

verifyEcdsaMintWitnessV3
  :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyEcdsaMintWitnessV3 era =
  ( policyIdV3 verifyEcdsaPolicyV3
  , mintScriptWitness era plutusL3 (Left verifyEcdsaPolicyScriptV3) verifyEcdsaRedeemer
  )
