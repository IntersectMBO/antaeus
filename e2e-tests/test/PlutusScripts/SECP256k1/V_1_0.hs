{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.SECP256k1.V_1_0 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Helpers.ScriptUtils qualified as U
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Helpers (
  mintScriptWitness,
  plutusL1,
  plutusL2,
  policyIdV1,
  policyIdV2,
  toScriptData,
 )
import PlutusScripts.SECP256k1.Common (
  Secp256Params (..),
  ecdsaAssetName,
  schnorrAssetName,
  verifyEcdsaParams,
  verifyEcdsaRedeemer,
  verifySchnorrParams,
 )
import PlutusTx qualified as P
import PlutusTx.Builtins qualified as P
import PlutusTx.Prelude qualified as P

-- Schnorr minting policy --

verifySchnorrPolicy :: SerialisedScript
verifySchnorrPolicy =
  serialiseCompiledCode $
    $$(P.compile [||mkVerifySchnorrPolicy||])
      `P.unsafeApplyCode` P.liftCode
        plcVersion100
        verifySchnorrParams
  where
    {-# INLINEABLE mkVerifySchnorrPolicy #-}
    mkVerifySchnorrPolicy :: Secp256Params -> P.BuiltinData -> P.BuiltinData -> ()
    mkVerifySchnorrPolicy Secp256Params{..} _redeemer _sc =
      U.check $ P.verifySchnorrSecp256k1Signature vkey msg sig

verifySchnorrPolicyScriptV1 :: C.PlutusScript C.PlutusScriptV1
verifySchnorrPolicyScriptV1 = C.PlutusScriptSerialised verifySchnorrPolicy

verifySchnorrPolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
verifySchnorrPolicyScriptV2 = C.PlutusScriptSerialised verifySchnorrPolicy

verifySchnorrAssetIdV1 :: C.AssetId
verifySchnorrAssetIdV1 =
  C.AssetId (policyIdV1 verifySchnorrPolicy) schnorrAssetName

verifySchnorrAssetIdV2 :: C.AssetId
verifySchnorrAssetIdV2 =
  C.AssetId (policyIdV2 verifySchnorrPolicy) schnorrAssetName

verifySchnorrMintWitnessV1
  :: C.ShelleyBasedEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifySchnorrMintWitnessV1 sbe =
  ( policyIdV1 verifySchnorrPolicy
  , mintScriptWitness
      sbe
      plutusL1
      (Left verifySchnorrPolicyScriptV1)
      (toScriptData ())
  )

verifySchnorrMintWitnessV2
  :: C.ShelleyBasedEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifySchnorrMintWitnessV2 sbe =
  ( policyIdV2 verifySchnorrPolicy
  , mintScriptWitness
      sbe
      plutusL2
      (Left verifySchnorrPolicyScriptV2)
      (toScriptData ())
  )

-- ECDSA minting policy --

verifyEcdsaPolicy :: SerialisedScript
verifyEcdsaPolicy =
  serialiseCompiledCode $
    $$(P.compile [||mkVerifyEcdsaPolicy||])
      `P.unsafeApplyCode` P.liftCode plcVersion100 verifyEcdsaParams
  where
    {-# INLINEABLE mkVerifyEcdsaPolicy #-}
    mkVerifyEcdsaPolicy :: Secp256Params -> P.BuiltinData -> P.BuiltinUnit
    mkVerifyEcdsaPolicy Secp256Params{..} _sc =
      P.check $ P.verifyEcdsaSecp256k1Signature vkey msg sig

verifyEcdsaPolicyScriptV1 :: C.PlutusScript C.PlutusScriptV1
verifyEcdsaPolicyScriptV1 = C.PlutusScriptSerialised verifyEcdsaPolicy

verifyEcdsaPolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
verifyEcdsaPolicyScriptV2 = C.PlutusScriptSerialised verifyEcdsaPolicy

verifyEcdsaAssetIdV1 :: C.AssetId
verifyEcdsaAssetIdV1 = C.AssetId (policyIdV1 verifyEcdsaPolicy) ecdsaAssetName

verifyEcdsaAssetIdV2 :: C.AssetId
verifyEcdsaAssetIdV2 = C.AssetId (policyIdV2 verifyEcdsaPolicy) ecdsaAssetName

verifyEcdsaMintWitnessV1
  :: C.ShelleyBasedEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyEcdsaMintWitnessV1 sbe =
  ( policyIdV1 verifyEcdsaPolicy
  , mintScriptWitness
      sbe
      plutusL1
      (Left verifyEcdsaPolicyScriptV1)
      verifyEcdsaRedeemer
  )

verifyEcdsaMintWitnessV2
  :: C.ShelleyBasedEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyEcdsaMintWitnessV2 sbe =
  ( policyIdV2 verifyEcdsaPolicy
  , mintScriptWitness
      sbe
      plutusL2
      (Left verifyEcdsaPolicyScriptV2)
      verifyEcdsaRedeemer
  )
